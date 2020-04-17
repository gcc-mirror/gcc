/* C++ modules.  Experimental!
   Copyright (C) 2018-2020 Free Software Foundation, Inc.
   Written by Nathan Sidwell <nathan@acm.org> while at FaceBook

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* The C++ module mapper server.  

   stdin/out
   bidirectional or pair of unidirectional fds
   local socket
   ipv6 socket
 */

#include "config.h"

/* Include network stuff first.  Excitingly OSX10.14 uses bcmp here, which
   we poison later!  */
#if defined (HAVE_AF_UNIX) || defined (HAVE_AF_INET6)
/* socket, bind, listen, accept{4}  */
# define NETWORKING 1
# include <sys/socket.h>
# ifdef HAVE_AF_UNIX
/* sockaddr_un  */
#  include <sys/un.h>
# endif
# include <netinet/in.h>
# ifdef HAVE_AF_INET6
/* sockaddr_in6, getaddrinfo, freeaddrinfo, gai_sterror, ntohs, htons.  */
#  include <netdb.h>
# endif
#ifdef HAVE_INET_NTOP
/* inet_ntop.  */
#include <arpa/inet.h>
#endif
#endif
#ifndef HAVE_AF_INET6
# define gai_strerror(X) ""
#endif

#ifdef NETWORKING
#ifdef HAVE_EPOLL
/* epoll_create, epoll_ctl, epoll_pwait  */
#include <sys/epoll.h>
#endif
#if defined (HAVE_PSELECT) || defined (HAVE_SELECT)
/* pselect or select  */
#include <sys/select.h>
#endif
#endif

#define INCLUDE_VECTOR
#define INCLUDE_MAP
#define INCLUDE_SET
#include "system.h"
#include "version.h"
#include "intl.h"
#include <getopt.h>

#ifndef HAVE_MEMRCHR
/* Some unfortunate souls do not have memrchr.
   Everyone is fighting a struggle you know nothing about.  */
static void *
memrchr (void *s_, int c, size_t n)
{
  unsigned char *s = (unsigned char *)s_;
  while (n--)
    if (s[n] == c)
      return &s[n];
  return NULL;
}
#endif
#ifndef HAVE_SIGHANDLER_T
typedef void (*sighandler_t) (int);
#endif

/* Header names are obvious pathnames -- absolute, or ./  */
#define IS_HEADER_NAME(STR) \
  (IS_ABSOLUTE_PATH (STR) || ((STR)[0] == '.' && IS_DIR_SEPARATOR ((STR)[1])))

#define DOT_REPLACE ','
#define COLON_REPLACE '-'

/* Mapper Protocol version.  Very new.  */
#define MAPPER_VERSION 0

const char *progname;

/* Speak thoughts out loud.  */
static bool flag_noisy = false;

/* One and done.  */
static bool flag_one = false;

/* Serialize connections.  */
static bool flag_sequential = false;

/* Fallback to default if map file is unrewarding.  */
static bool flag_fallback = false;

/* Root binary directory.  */
static const char *flag_root = "gcm.cache";

/* String comparison for the mapper map.  */
struct string_cmp {
  bool operator() (const char *a, const char *b) const
  {
    return strcmp (a, b) < 0;
  }
};
typedef std::map<const char *, const char *, string_cmp> module_map_t;

struct netmask {
  in6_addr addr;
  unsigned bits;

  netmask (const in6_addr &a, unsigned b)
  {
    if (b > sizeof (in6_addr) * 8)
      b = sizeof (in6_addr) * 8;
    bits = b;
    unsigned byte = (b + 7) / 8;
    unsigned ix = 0;
    for (ix = 0; ix < byte; ix++)
      addr.s6_addr[ix] = a.s6_addr[ix];
    for (; ix != sizeof (in6_addr); ix++)
      addr.s6_addr[ix] = 0;
    if (b & 3)
      addr.s6_addr[b/7] &= (255 << 8) >> (b & 3);
  }

  bool includes (const in6_addr &a) const
  {
    unsigned byte = bits / 8;
    for (unsigned ix = 0; ix != byte; ix++)
      if (addr.s6_addr[ix] != a.s6_addr[ix])
	return false;
    if (bits & 3)
      if ((addr.s6_addr[byte] ^ a.s6_addr[byte]) >> (8 - (bits & 3)))
	return false;
    return true;
  }
};

/* Netmask comparison.  */
struct netmask_cmp {
  bool operator() (const netmask &a, const netmask &b) const
  {
    if (a.bits != b.bits)
      return a.bits < b.bits;
    for (unsigned ix = 0; ix != sizeof (in6_addr); ix++)
      if (a.addr.s6_addr[ix] != b.addr.s6_addr[ix])
	return a.addr.s6_addr[ix] < b.addr.s6_addr[ix];
    return false;
  }
};

typedef std::set<netmask, netmask_cmp> netmask_set_t;
typedef std::vector<netmask> netmask_vec_t;

/* If there is a known mapping, this is it.  */
static module_map_t *module_map;

static netmask_set_t netmask_set;

static netmask_vec_t accept_addrs;

using namespace std;

/* Progress messages to the user.  */
static bool ATTRIBUTE_PRINTF_1 ATTRIBUTE_COLD
noisy (const char *fmt, ...)
{
  fprintf (stderr, "%s:", progname);
  va_list args;
  va_start (args, fmt);
  vfprintf (stderr, fmt, args);
  va_end (args);
  fprintf (stderr, "\n");

  return false;
}

/* More messages to the user.  */

static void ATTRIBUTE_PRINTF_2
fnotice (FILE *file, const char *fmt, ...)
{
  va_list args;

  va_start (args, fmt);
  vfprintf (file, _(fmt), args);
  va_end (args);
}

/* Strip out the source directory from FILE.  */

static const char *
trim_src_file (const char *file)
{
  static const char me[] = __FILE__;
  unsigned pos = 0;

  while (file[pos] == me[pos] && me[pos])
    pos++;
  while (pos && !IS_DIR_SEPARATOR (me[pos-1]))
    pos--;

  return file + pos;
}

/* Die screaming.  */

void ATTRIBUTE_NORETURN ATTRIBUTE_PRINTF_1 ATTRIBUTE_COLD
internal_error (const char *fmt, ...)
{
  fprintf (stderr, "%s:Internal error ", progname);
  va_list args;

  va_start (args, fmt);
  vfprintf (stderr, fmt, args);
  va_end (args);
  fprintf (stderr, "\n");

  exit (FATAL_EXIT_CODE);
}

/* Hooked to from gcc_assert & gcc_unreachable.  */

void ATTRIBUTE_NORETURN ATTRIBUTE_COLD
fancy_abort (const char *file, int line, const char *func)
{
  internal_error ("in %s, at %s:%d", func, trim_src_file (file), line);
}

/* Exploded on a signal.  */

static void ATTRIBUTE_NORETURN ATTRIBUTE_COLD
crash_signal (int sig)
{
  signal (sig, SIG_DFL);
  internal_error ("signal %s", strsignal (sig));
}

/* A fatal error of some kind.  */

static void ATTRIBUTE_NORETURN ATTRIBUTE_COLD ATTRIBUTE_PRINTF_1
error (const char *msg, ...)
{
  fprintf (stderr, "%s:error: ", progname);
  va_list args;

  va_start (args, msg);
  vfprintf (stderr, msg, args);
  va_end (args);
  fprintf (stderr, "\n");

  exit (1);
}

/* Figure out the BMI from a module name.  Returns NULL if we do not
   know.  */

const char *
module2bmi (const char *module)
{
  const char *res = NULL;

  if (module_map)
    {
      module_map_t::iterator iter = module_map->find (module);
      res = iter != module_map->end () ? iter->second : NULL;
    }

  if (res && res[0] == '=')
    /* An include alias.  */
    res = NULL;

  if (!res && flag_fallback)
    {
      static char *workspace;
      static unsigned alloc = 0;
      unsigned l = strlen (module);

      if (alloc < l + 10)
	{
	  alloc = l + 20;
	  workspace = XRESIZEVEC (char, workspace, alloc);
	}

      bool is_header = IS_HEADER_NAME (module);
      bool is_abs = is_header && module[0] != '.';
      memcpy (workspace + is_abs, module, l);
      l += is_abs;
      workspace[l] = 0;
      if (is_header)
	{
	  workspace[0] = is_abs ? '.' : DOT_REPLACE;

	  /* Map .. to !!.  */
	  for (unsigned ix = 0; ix != l; ix++)
	    if (IS_DIR_SEPARATOR (workspace[ix])
		&& workspace[ix + 1] == '.'
		&& workspace[ix + 2] == '.')
	      {
		workspace[ix + 1] = DOT_REPLACE;
		workspace[ix + 2] = DOT_REPLACE;
	      }
	}
      else if (COLON_REPLACE != ':')
	if (char *colon = (char *)memchr (workspace, ':', l))
	  *colon = COLON_REPLACE;

      strcpy (workspace + l, ".gcm");
      res = workspace;
    }

  return res;
}

/* Read or write buffer from/to the compiler.  */

class buffer {
  char *buf;
  size_t size;
  char *pos;
  char *end;
  char *start;
  int fd;
  bool cork_p;
  bool bol_p;
  bool last_p;

public:
  buffer (int fd)
    : buf (NULL), size (200),
      pos (NULL), end (NULL), start (NULL), fd (fd)
  {
    buf = XNEWVEC (char, size);
    init ();
  }
  ~buffer ()
  {
    XDELETEVEC (buf);
  }

public:
  int get_fd () const
  {
    return fd;
  }

public:
  bool corking () const
  {
    return cork_p;
  }
  void cork (bool cork)
  {
    cork_p = cork;
  }
  bool empty () const
  {
    return end == buf;
  }

public:
  void init ()
  {
    end = pos = buf;
    bol_p = true;
    cork_p = false;
    last_p = false;
  }
  int do_read (unsigned);
  int do_write (unsigned);

public:
  void ATTRIBUTE_PRINTF_3 send_response (unsigned, const char *fmt, ...);
  bool get_request (unsigned id, bool first);
  bool get_request (unsigned id, FILE *);

public:
  void request_unexpected (unsigned id);
  const char *request_error ()
  {
    const char *result = pos != end ? pos : "unspecified error";
    pos = end;
    return result;
  }
  bool eol_p () const
  {
    return pos == end;
  }
  bool get_eol (unsigned id, bool ignore = false);
  char *get_token (unsigned id, bool all = false);
  int get_word (const char *token, const char *option, ...);
  int get_word (unsigned id, const char *option, ...);

private:
  int get_word (const char *token, const char *option, va_list args);

public:
  void close (buffer const *other)
  {
    if (!other || other->fd != fd)
      ::close (fd);
    fd = -1;
  }
};

/* Read from the FD.  If we complete a (batched) line, return
   non-zero.  Also non-zero on error.  */

int
buffer::do_read (unsigned id)
{
  unsigned off = end - buf;
  if (off == size)
    {
      size *= 2;
      buf = XRESIZEVEC (char, buf, size);
      end = buf + off;
    }
  pos = end;

  int bytes = ::read (fd, buf + off, size - off);
  if (bytes < 0)
    {
      noisy ("%u:read fail: %s", id, xstrerror (errno));
      return -1;
    }
  else if (!bytes)
    {
      flag_noisy && noisy ("%u:end of file", id);
      return -1;
    }

  while (bytes)
    {
      if (bol_p)
	{
	  if (buf[off] == '+')
	    cork_p = true;
	  else
	    last_p = true;
	}
      if (char *eol = (char *)memchr (buf + off, '\n', size - off))
	{
	  bol_p = true;
	  unsigned nline = eol + 1 - buf;
	  bytes -= nline - off;
	  off = nline;
	}
      else
	{
	  bol_p = false;
	  off += bytes;
	  bytes = 0;
	  break;
	}
    }

  end = buf + off + bytes;

  if (bol_p && last_p)
    {
      *--end = 0;
      flag_noisy && noisy ("%u:read %s: '%s'", id, cork_p ? "batch" : "line",
			   buf);
      start = buf;
      return +1;
    }

  flag_noisy && noisy ("%u:read partial '%.*s'", id, int (end - pos), pos);
  return 0;
}

/* Write (batched) line to FD.  If completed return non-zero.  Also
   non-zero on error.  */

int
buffer::do_write (unsigned id)
{
  unsigned len = end - pos;
  int bytes = ::write (fd, pos, len);
  if (bytes < 0)
    {
      noisy ("%u:write fail: %s", id, xstrerror (errno));
      return -1;
    }
  else if (!bytes)
    {
      flag_noisy && noisy ("%u:end of file", id);
      return -1;
    }
  else
    pos += bytes;

  return pos == end;
}

/* Format a response into the buffer.  Pay attention to the corking
   state.  */

void
buffer::send_response (unsigned id, const char *fmt, ...)
{
  gcc_assert (unsigned (end - buf + 2) <= size);
  if (cork_p)
    *end++ = '+';
  else if (end != buf)
    *end++ = '-';

  for (;;)
    {
      va_list args;
      va_start (args, fmt);
      size_t available = (buf + size) - end;
      gcc_checking_assert (available);
      unsigned actual = vsnprintf (end, available, fmt, args);
      va_end (args);
      if (actual + 2 < available)
	{
	  flag_noisy && noisy ("%u:response '%s'", id, end - (end != buf));
	  end += actual;
	  break;
	}

      size = size * 2 + actual + 20;
      char *next = XRESIZEVEC (char, buf, size);
      end = next + (end - buf);
      buf = pos = next;
    }
  *end++ = '\n';
}

/* Prepare for the next request.  Sets corking appropriately if there
   is/isn't more.  FIRST indicates whether this is the first after
   completing a read.  */

bool
buffer::get_request (unsigned id, bool first)
{
  if (first)
    ;
  else if (cork_p)
    start = end + 1;
  else
    return false;

 again:
  pos = start;
  end = NULL;
  if (*pos == '+')
    {
      pos++;
      end = strchr (pos, '\n');
      cork_p = true;
    }

  if (!end)
    {
      cork_p = false;
      if (*pos == '-')
	pos++;
      end = pos + strlen (pos);
    }
  else
    *end = 0;

  while (*pos && ISSPACE (*pos))
    pos++;

  flag_noisy && noisy ("%u:request '%s%s'", id, cork_p ? "+" : "", pos);
  if (*pos)
    return true;
  if (cork_p)
    goto again;
  return false;;
}

/* Get a request from a file.  */

bool
buffer::get_request (unsigned id, FILE *file)
{
  init ();
  unsigned off = end - buf;
  do
    {
      if (off + 1 == size)
	{
	  size *= 2;
	  buf = XRESIZEVEC (char, buf, size);
	  end = buf + off;
	}

      if (!fgets (buf + off, size - off, file))
	{
	  if (ferror (file))
	    noisy ("%u:read fail: %s", id, xstrerror (errno));
	  return false;
	}

      off += strlen (buf + off);
    }
  while (off && buf[off - 1] != '\n');
  pos = buf;
  end = buf + off;
  *--end = 0;

  return true;
}

/* We didn't expect this.  */

void
buffer::request_unexpected (unsigned id)
{
  if (start)
    {
      /* Restore the whitespace we zapped tokenizing.  */
      for (char *ptr = start; ptr != pos; ptr++)
	if (!*ptr)
	  *ptr = ' ';
      noisy ("%u:mapper command malformed: %s", id, start);
    }
  pos = end;
}

/* Check we're at EOL.  Returns true, if that was the case.  */

bool
buffer::get_eol (unsigned id, bool ignore)
{
  bool at_end = eol_p ();
  if (!at_end && !ignore)
    request_unexpected (id);
  pos = end;
  return at_end;
}

/* Get a space-separated token.  ALL means to EOL.  */

char *
buffer::get_token (unsigned id, bool all)
{
  char *ptr = pos;

  if (ptr == end)
    {
      request_unexpected (id);
      ptr = NULL;
    }
  else if (all)
    pos = end;
  else
    {
      char *eptr = ptr;
      while (eptr != end && !ISSPACE (*eptr))
	eptr++;

      if (eptr != end)
	{
	  *eptr++ = 0;
	  while (eptr != end && ISSPACE (*eptr))
	    eptr++;
	}
      pos = eptr;
    }

  return ptr;
}

/* Return which OPTION TOKEN matches.  -1 if none.  */

int
buffer::get_word (const char *token, const char *option, va_list args)
{
  int count = 0;

  do
    {
      if (!strcmp (option, token))
	return count;
      count++;
      option = va_arg (args, const char *);
    }
  while (option);

  return -1;
}

/* Return which OPTION... TOKEN matches.  */

int
buffer::get_word (const char *token, const char *option, ...)
{
  va_list args;

  va_start (args, option);
  int res = get_word (token, option, args);
  va_end (args);

  return res;
}

/* Tokenize the request and return which it matched.  */

int
buffer::get_word (unsigned id, const char *option, ...)
{
  if (const char *token = get_token (id))
    {
      va_list args;

      va_start (args, option);
      int res = get_word (token, option, args);
      va_end (args);
      if (res < 0)
	request_unexpected (id);
      return res;
    }
  return -1;
}

char *
encode_module_name (const char *mod, const char *pfx = NULL)
{
  size_t mlen = strlen (mod);
  size_t plen = pfx ? strlen (pfx) : 0;
  char *key = XNEWVEC (char, plen + mlen + 1);
  char *ptr = key;
  if (pfx)
    {
      memcpy (ptr, pfx, plen);
      ptr += plen;
    }

  memcpy (ptr, mod, mlen + 1);

  return key;
}

/* Read the mapping file STREAM and populate the module_map from it.  */

void
read_mapping_file (FILE *stream, const char *cookie, bool starting)
{
  buffer buf (-1);
  size_t root_len = 0;

  while (buf.get_request (0, stream))
    {
      char *mod = buf.get_token (0);

      /* Ignore non-cookie lines.  */
      if (cookie)
	{
	  if (0 != strcmp (mod, cookie))
	    {
	      buf.get_eol (0, true);
	      continue;
	    }
	  mod = buf.get_token (0);
	}

      char *file = NULL;
      if (mod && !buf.eol_p ())
	file = buf.get_token (0, true);

      if (!buf.get_eol (0, false))
	continue;

      if (starting && 0 == strcmp (mod, "$root"))
	{
	  root_len = strlen (file);
	  char *root = XNEWVEC (char, root_len + 1);
	  flag_root = root;
	  memcpy (root, file, root_len + 1);
	  continue;
	}

      starting = false;
      if (flag_root && file && 0 == strncmp (file, flag_root, root_len)
	  && IS_DIR_SEPARATOR (file[root_len]))
	file += root_len + 1;

      mod = encode_module_name (mod);
      std::pair<module_map_t::iterator, bool> inserted
	= module_map->insert (module_map_t::value_type (mod, 0));

      if (!inserted.second)
	XDELETEVEC (mod);

      if (!inserted.first->second && file)
	{
	  if (file[0] == '=')
	    /* An alias.  */
	    file = encode_module_name (file + 1, "=");
	  else
	    file = xstrdup (file);
	  inserted.first->second = file;
	}
    }
}

/* A remote client.  */

class client {
public:
  enum state {
    HANDSHAKE, /* Expecting handshake.  */
    TALKING,   /* Regular state.  */
  };

private:
  buffer read;
  buffer write;
  const char *cookie;
  unsigned id;

public:
  unsigned ix;

private:
  int dir;   /* -1->write, +1->read, 0->waiting */
  enum state state;
  static unsigned id_ctr;

public:
  client (unsigned, int, int, const char *);
  ~client ();

public:
#if defined (HAVE_PSELECT) || defined (HAVE_SELECT)
  unsigned set_fd (fd_set *readers, fd_set *writers, unsigned limit)
  {
    unsigned fd = -1;
    fd_set *set = NULL;

    if (dir > 0)
      {
	set = readers;
	fd = read.get_fd ();
      }
    else if (dir < 0)
      {
	set = writers;
	fd = write.get_fd ();
      }

    if (set)
      {
	FD_SET (fd, set);
	if (limit <= fd)
	  limit = fd + 1;
      }

    return limit;
  }
  bool test_fd (fd_set *readers, fd_set *writers)
  {
    unsigned fd = -1;
    fd_set *set = NULL;

    if (dir > 0)
      {
	set = readers;
	fd = read.get_fd ();
      }
    else if (dir < 0)
      {
	set = writers;
	fd = write.get_fd ();
      }
    gcc_assert (set);
    return FD_ISSET (fd, set);
  }
#endif

private:
  void imex_response (unsigned id, const char *module, bool deferred);

public:
  bool process (int = -1);
  int action ();
  unsigned close (int = -1);
};

unsigned client::id_ctr = 0;

client::client (unsigned ix, int from, int to, const char *cookie)
  : read (from), write (to), cookie (cookie),
    id ((id_ctr += (from == to))), ix (ix), dir (+1), state (HANDSHAKE)
{
  flag_noisy && noisy ("%u:created", id);
}

client::~client ()
{
  flag_noisy && noisy ("%u:destroyed", id);
  gcc_assert (read.get_fd () < 0 && write.get_fd () < 0);
}

/* Manipulate the EPOLL state, or do nothing, if there is epoll.  */

#ifdef HAVE_EPOLL
static inline void
do_epoll_ctl (int epoll_fd, int code, int event, int fd, client *client)
{
  epoll_event ev;
  ev.events = event;
  ev.data.ptr = client;
  if (epoll_ctl (epoll_fd, code, fd, &ev))
    {
      noisy ("epoll_ctl error:%s", xstrerror (errno));
      gcc_unreachable ();
    }
}
#define my_epoll_ctl(EFD,C,EV,FD,CL) \
  ((EFD) >= 0 ? do_epoll_ctl (EFD,C,EV,FD,CL) : (void)0)
#else
#define  my_epoll_ctl(EFD,C,EV,FD,CL) ((void)(EFD), (void)(FD), (void)CL)
#endif

unsigned
client::close (int epoll_fd)
{
  if (epoll_fd >= 0)
    {
      gcc_assert (read.get_fd () == write.get_fd ());
      my_epoll_ctl (epoll_fd, EPOLL_CTL_DEL, EPOLLIN, read.get_fd (), NULL);
    }
  write.close (&read);
  read.close (NULL);
  return ix;
}

/* We can do something.  Do it.  */

bool
client::process (int epoll_fd)
{
  gcc_assert (dir);

  int e = dir < 0 ? write.do_write (id) : read.do_read (id);
  if (e < 0)
    /* We're boned.  */
    return true;

  if (e)
    {
      int new_dir = dir > 0 ? action () : +1;
      if (dir != new_dir)
	{
	  /* We've changed dir.  */
	  if (epoll_fd >= 0)
	    {
#ifdef HAVE_EPOLL
	      int code = (!new_dir ? EPOLL_CTL_DEL
			  : !dir ? EPOLL_CTL_ADD
			  : EPOLL_CTL_MOD);
#endif
	      gcc_assert (read.get_fd () == write.get_fd ());
	      my_epoll_ctl (epoll_fd, code, new_dir >= 0 ? EPOLLIN : EPOLLOUT,
			    read.get_fd (), this);
	    }
	  dir = new_dir;
	}
      if (dir > 0)
	{
	  /* Initialize for new request.  */
	  write.init ();
	  read.init ();
	}
    }

  return false;
}

void
client::imex_response (unsigned id, const char *name, bool deferred)
{
  const char *sfx = &" "[!deferred];
  const char *pfx = deferred ? name : sfx;
  if (const char *bmi = module2bmi (name))
    write.send_response (id, "%s%sOK %s", pfx, sfx, bmi);
  else
    write.send_response (id, "%s%sERROR Unknown module", pfx, sfx);
}

/* We completed a read.  Do whatever processing we should.  */

int
client::action ()
{
  flag_noisy && noisy ("Actioning client %u", id);
  bool batched = false;

  for (bool first = true; read.get_request (id, first); first = false)
    {
      if (read.corking ())
	{
	  if (!batched)
	    {
	      batched = true;
	      write.cork (true);
	    }
	}
      else if (batched)
	write.cork (false);

      switch (state)
	{
	case HANDSHAKE:
	  /* HELLO $version $flags $cookie  */
	  switch (read.get_word (id, "HELLO", NULL))
	    {
	    case 0:
	      {
		char *ver_str = read.get_token (id);
		char *eptr;
		int ver = strtol (ver_str, &eptr, 0);
		const char *err = NULL;
		if (*eptr || ver != MAPPER_VERSION)
		  err = "ERROR Expected version %d";

		int queries = read.get_word (id, "GCC", "SOURCE", NULL);
		if (queries < 0)
		  {
		    if (!err)
		      err = "ERROR Unknown query type";
		  }
		else if (queries != 0)
		  {
		    if (!err)
		      err = "ERROR Only GCC queries supported";
		  }

		char *ckie = read.get_token (id, true);
		if (cookie && 0 != strcmp (cookie, ckie))
		  {
		    if (!err)
		      err = "ERROR Cookie mismatch";
		  }
		gcc_assert (read.get_eol (id));
		write.send_response (id, err ? err : "HELLO %d GCC %s",
					 MAPPER_VERSION, flag_root);
		if (!err)
		  state = TALKING;
		break;
	      }
	      break;

	    default:
	      read.get_eol (id);
	      write.send_response (id, "ERROR Expecting HELLO");
	      break;
	    }
	  break;

	case TALKING:
	  {
	    int word = read.get_word (id, "IMPORT", "EXPORT", "DONE",
				      "INCLUDE", NULL);
	    switch (word)
	      {
	      case 0: /* IMPORT */
	      case 1: /* EXPORT */
	      case 2: /* DONE */
	      case 3: /* INCLUDE */
		{
		  char *module = read.get_token (id);
		  read.get_eol (id);
		  switch (word)
		    {
		    case 0:
		    case 1:
		      imex_response (id, module, false);
		      break;
		    case 2:
		      /* No response.  */
		      break;

		    case 3:
		      {
			bool do_imp = false;
			if (module_map)
			  {
			    module_map_t::iterator iter
			      = module_map->find (module);
			    if (iter != module_map->end ())
			      do_imp = true;
			  }
			if (!do_imp)
			  /* See if the BMI exists.  */
			  // FIXME: We should have syntax for this in
			  // a config file?  This is unpleasant.
			  if (const char *bmi = module2bmi (module))
			    {
			      size_t l = strlen (bmi);
			      size_t r = strlen (flag_root);
			      char *buf = new char [l + r + 2];
			      memcpy (buf, flag_root, r);
			      buf[r] = DIR_SEPARATOR;
			      memcpy (buf + r + 1, bmi, l + 1);
			      int fd = ::open (buf, O_RDONLY);
			      delete[] buf;
			      if (fd >= 0)
				{
				  ::close (fd);
				  do_imp = true;
				}
			    }
			write.send_response (id, do_imp ? "IMPORT" : "TEXT");
		      }
		      break;

		    default: gcc_unreachable ();
		    }
		  
		}
		break;

	      default:
		read.get_eol (id, true);
		write.send_response (id, "ERROR Unknown request");
		break;
	      }
	  }
	break;

	default:
	  gcc_unreachable ();
	}
    }

  if (write.corking ())
    {
      write.cork (false);
      /* We need to send a blank response to finish the batched
	 responses.  The compiler complains about a zero-size printf
	 format, so hide it inside a zero-length string.  */
      write.send_response (id, "%s", "");
    }

  return write.empty () ? +1 : -1;
}

/* A single client communicating over FROM & TO.  */

static void
singleton (int from, int to, const char *cookie)
{
  client me (0, from, to, cookie);

  while (!me.process ())
    continue;

  me.close ();
}

#ifdef NETWORKING
/* We increment this to tell the server to shut down.  */
static volatile int term = false;
static volatile int kill_sock_fd = -1;
#if !defined (HAVE_PSELECT) && defined (HAVE_SELECT)
static int term_pipe[2] = {-1, -1};
#else
#define term_pipe ((int *)NULL)
#endif

/* A terminate signal.  Shutdown gracefully.  */

static void
term_signal (int sig)
{
  signal (sig, term_signal);
  term = term + 1;
  if (term_pipe && term_pipe[1] >= 0)
    write (term_pipe[1], &term_pipe[1], 1);
}

/* A kill signal.  Shutdown immediately.  */

static void
kill_signal (int sig)
{
  signal (sig, SIG_DFL);
  int sock_fd = kill_sock_fd;
  if (sock_fd >= 0)
    close (sock_fd);
  exit (2);
}

/* A server listening on bound socket SOCK_FD.  */

static void
server (bool ip6, int sock_fd, const char *cookie)
{
  /* I don't know what a good listen queue length might be.  */
  if (listen (sock_fd, flag_one ? 1 : 5))
    error ("cannot listen: %s", xstrerror (errno));

  int epoll_fd = -1;
  unsigned live = 0;
  vector<client *> clients (20);
  unsigned reuse = 0;

  signal (SIGTERM, term_signal);
#ifdef HAVE_EPOLL
  epoll_fd = epoll_create (1);
#endif
  if (epoll_fd >= 0)
    my_epoll_ctl (epoll_fd, EPOLL_CTL_ADD, EPOLLIN, sock_fd, NULL);

#if defined (HAVE_EPOLL) || defined (HAVE_PSELECT) || defined (HAVE_SELECT)
  sigset_t mask;
  {
    sigset_t block;
    sigemptyset (&block);
    sigaddset (&block, SIGTERM);
    sigprocmask (SIG_BLOCK, &block, &mask);
  }
#endif

#ifdef HAVE_EPOLL
  const unsigned max_events = 20;
  epoll_event events[max_events];
#endif
#if defined (HAVE_PSELECT) || defined (HAVE_SELECT)
  fd_set readers, writers;
  unsigned select_ix = 0;
#endif
  int event_count = 0;
  if (term_pipe)
    pipe (term_pipe);

  for (client *actionable = NULL; live || sock_fd >= 0;)
    {
      /* Wait for an event.  */
      bool eintr = false;
      if (!event_count)
	{
	  if (epoll_fd >= 0)
#ifdef HAVE_EPOLL
	    event_count = epoll_pwait (epoll_fd, events, max_events, -1, &mask);
#endif
	  if (epoll_fd < 0)
	    {
#if defined (HAVE_PSELECT) || defined (HAVE_SELECT)
	      FD_ZERO (&readers);
	      FD_ZERO (&writers);

	      unsigned limit = 0;
	      if (sock_fd >= 0
		  && !(term || (live && (flag_one || flag_sequential))))
		{
		  FD_SET (sock_fd, &readers);
		  limit = sock_fd + 1;
		}
	      if (term_pipe && term_pipe[0] >= 0)
		{
		  FD_SET (term_pipe[0], &readers);
		  if (unsigned (term_pipe[0]) >= limit)
		    limit = term_pipe[0] + 1;
		}
	      select_ix = clients.size ();
	      for (unsigned ix = select_ix; ix--;)
		if (client *c = clients[ix])
		  limit = c->set_fd (&readers, &writers, limit);
#ifdef HAVE_PSELECT
	      event_count = pselect (limit, &readers, &writers, NULL,
				     NULL, &mask);
#else
	      event_count = select (limit, &readers, &writers, NULL, NULL);
#endif
	      if (term_pipe && FD_ISSET (term_pipe[0], &readers))
		{
		  /* Fake up an interrupted system call.  */
		  event_count = -1;
		  errno = EINTR;
		}
#endif
	    }

	  if (event_count < 0)
	    {
	      actionable = NULL;
	      if (errno == EINTR)
		eintr = true;
	      else
		error ("cannot %s: %s", epoll_fd >= 0 ? "epoll_wait"
#ifdef HAVE_PSELECT
		       : "pselect",
#else
		       : "select",
#endif
		       xstrerror (errno));
	      event_count = 0;
	    }

	  if (event_count > 0)
	    {
#ifdef HAVE_EPOLL
	      if (epoll_fd >= 0)
		{
		  /* See PR c++/88664 for why a temporary is used.  */
		  void *data = events[--event_count].data.ptr;
		  actionable = static_cast <client *> (data);
		}
#endif
#if defined (HAVE_PSELECT) || defined (HAVE_SELECT)
	      if (epoll_fd < 0)
		{
		  actionable = NULL;
		  while (select_ix && !actionable)
		    {
		      if (client *c = clients[--select_ix])
			if (c->test_fd (&readers, &writers))
			  actionable = c;
		    }
		  gcc_assert (actionable || term
			      || FD_ISSET (sock_fd, &readers));
		  event_count--;
		}
#endif
	      gcc_assert (!actionable
			  || (actionable->ix < clients.size ()
			      && clients[actionable->ix] == actionable));
	    }
	}

      if (eintr)
	flag_noisy && noisy ("Interrupted wait");
      if (!actionable && !eintr && sock_fd >= 0)
	{
	  /* A new client.  */
	  sockaddr_in6 addr;
	  socklen_t addr_len = sizeof (addr);

#ifdef HAVE_ACCEPT4
	  int client_fd = accept4 (sock_fd, ip6 ? (sockaddr *)&addr : NULL,
				   &addr_len, SOCK_NONBLOCK);
#else
	  int client_fd = accept (sock_fd, ip6 ? (sockaddr *)&addr : NULL,
				  &addr_len);
#endif
	  if (client_fd < 0)
	    {
	      error ("cannot accept: %s", xstrerror (errno));
	      flag_one = true;
	    }

	  if (ip6)
	    {
	      const char *str = NULL;
#if HAVE_INET_NTOP
	      char name[INET6_ADDRSTRLEN];
	      str = inet_ntop (addr.sin6_family, &addr.sin6_addr,
			       name, sizeof (name));
#endif
	      if (!accept_addrs.empty ())
		{
		  netmask_vec_t::iterator e = accept_addrs.end ();
		  for (netmask_vec_t::iterator i = accept_addrs.begin ();
		       i != e; ++i)
		    if (i->includes (addr.sin6_addr))
		      goto present;
		  close (client_fd);
		  client_fd = -1;
		  noisy ("Rejecting connection from disallowed source '%s'",
			 str ? str : "");
		  continue;
		present:;
		}
	      flag_noisy && noisy ("Accepting connection from '%s'",
				   str ? str : "");
	    }

#if !defined (HAVE_ACCEPT4) \
  && (defined (HAVE_EPOLL) || defined (HAVE_PSELECT) || defined (HAVE_SELECT))
	  int flags = fcntl (client_fd, F_GETFL, 0);
	  fcntl (client_fd, F_SETFL, flags | O_NONBLOCK);
#endif
	  for (; reuse != clients.size (); reuse++)
	    if (!clients[reuse])
	      break;
	  if (reuse == clients.size ())
	    clients.push_back (NULL);

	  actionable = new client (reuse, client_fd, client_fd, cookie);
	  clients[reuse] = actionable;
	  my_epoll_ctl (epoll_fd, EPOLL_CTL_ADD, EPOLLIN,
			client_fd, clients[reuse]);
	  live++;
#if defined (HAVE_SELECTP) || defined (HAVE_EPOLL)
	  actionable = NULL;
#endif
	}

      if (sock_fd >= 0 && (term || flag_one || flag_sequential))
	{
	  /* Stop paying attention to sock_fd.  */
	  my_epoll_ctl (epoll_fd, EPOLL_CTL_DEL, EPOLLIN, sock_fd, NULL);
	  if (flag_one || term)
	    {
	      close (sock_fd);
	      sock_fd = -1;
	    }
	}

      if (actionable && actionable->process (epoll_fd))
	{
	  unsigned ix = actionable->close (epoll_fd);
	  if (ix < reuse)
	    reuse = ix;
	  clients[ix] = NULL;
	  delete actionable;
	  actionable = NULL;
	  live--;
	  if (flag_sequential)
	    my_epoll_ctl (epoll_fd, EPOLL_CTL_ADD, EPOLLIN, sock_fd, NULL);
	}
    }

#if defined (HAVE_EPOLL) || defined (HAVE_PSELECT) || defined (HAVE_SELECT)
  /* Restore the signal mask.  */
  sigprocmask (SIG_SETMASK, &mask, NULL);
#endif

  gcc_assert (sock_fd < 0);
  if (epoll_fd >= 0)
    close (epoll_fd);

  if (term_pipe && term_pipe[0] >= 0)
    {
      close (term_pipe[0]);
      close (term_pipe[1]);
    }
}
#endif

/* Print a usage message and exit.  If ERROR_P is nonzero, this is an error,
   otherwise the output of --help.  */

static void ATTRIBUTE_NORETURN
print_usage (int error_p)
{
  FILE *file = error_p ? stderr : stdout;
  int status = error_p ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE;

  fnotice (file, "Usage: cxx-mapper [OPTION...] [CONNECTION] [MAPPINGS...] \n\n");
  fnotice (file, "C++ Module Mapper.\n\n");
  fnotice (file, "  -a, --accept     Netmask to accept from\n");
  fnotice (file, "  -f, --fallback   Use fallback for missing mappings\n");
  fnotice (file, "  -h, --help       Print this help, then exit\n");
  fnotice (file, "  -n, --noisy      Print progress messages\n");
  fnotice (file, "  -1, --one        One connection and then exit\n");
  fnotice (file, "  -r, --root DIR   Root compiled module directory\n");
  fnotice (file, "  -s, --sequential Process connections sequentially\n");
  fnotice (file, "  -v, --version    Print version number, then exit\n");
  fnotice (file, "Send SIGTERM(%d) to terminate\n", SIGTERM);
  fnotice (file, "\nFor bug reporting instructions, please see:\n%s.\n",
	   bug_report_url);
  exit (status);
}

/* Print version information and exit.  */

static void ATTRIBUTE_NORETURN
print_version (void)
{
  fnotice (stdout, "cxx-mapper %s%s\n", pkgversion_string, version_string);
  fprintf (stdout, "Copyright %s 2018 Free Software Foundation, Inc.\n",
	   _("(C)"));
  fnotice (stdout,
	   _("This is free software; see the source for copying conditions.\n"
	     "There is NO warranty; not even for MERCHANTABILITY or \n"
	     "FITNESS FOR A PARTICULAR PURPOSE.\n\n"));
  exit (SUCCESS_EXIT_CODE);
}

/* ARG is a netmask to accept from.  Add it to the table.  Return
   false if we fail to resolve it.  */

static bool
accept_from (char *arg)
{
  bool ok = true;
#if HAVE_AF_INET6
  unsigned bits = sizeof (in6_addr) * 8;
  char *slash = strrchr (arg, '/');
  if (slash)
    {
      *slash = 0;
      if (slash[1])
	{
	  char *endp;
	  bits = strtoul (slash + 1, &endp, 0);
	}
    }

  addrinfo hints;

  hints.ai_flags = AI_NUMERICSERV;
  hints.ai_family = AF_INET6;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_protocol = 0;
  hints.ai_addrlen = 0;
  hints.ai_addr = NULL;
  hints.ai_canonname = NULL;
  hints.ai_next = NULL;

  struct addrinfo *addrs = NULL;
  if (int e = getaddrinfo (slash == arg ? NULL : arg, "0", &hints, &addrs))
    {
      noisy ("cannot resolve '%s': %s", arg, gai_strerror (e));
      ok = false;
    }
  else
    for (addrinfo *next = addrs; next; next = next->ai_next)
      if (next->ai_family == AF_INET6)
	{
	  netmask mask (((const sockaddr_in6 *)next->ai_addr)->sin6_addr, bits);
	  netmask_set.insert (mask);
	}
  freeaddrinfo (addrs);
#endif
  return ok;
}

/* Process args, return index to first non-arg.  */

static int
process_args (int argc, char **argv)
{
  static const struct option options[] =
    {
     { "accept", required_argument, NULL, 'a' },
     { "fallback",no_argument,	NULL, 'f' },
     { "help",	no_argument,	NULL, 'h' },
     { "noisy",	no_argument,	NULL, 'n' },
     { "one",	no_argument,	NULL, '1' },
     { "root",	required_argument, NULL, 'r' },
     { "sequential", no_argument, NULL, 's' },
     { "version", no_argument,	NULL, 'v' },
     { 0, 0, 0, 0 }
    };
  int opt;
  bool bad_accept = false;
  const char *opts = "a:fhn1r:sv";
  while ((opt = getopt_long (argc, argv, opts, options, NULL)) != -1)
    {
      switch (opt)
	{
	case 'a':
	  if (!accept_from (optarg))
	    bad_accept = true;
	  break;
	case 'f':
	  flag_fallback = true;
	  break;
	case 'h':
	  print_usage (false);
	  /* print_usage will exit.  */
	case 'n':
	  flag_noisy = true;
	  break;
	case '1':
	  flag_one = true;
	  break;
	case 'r':
	  flag_root = optarg;
	  break;
	case 's':
	  flag_sequential = true;
	  break;
	case 'v':
	  print_version ();
	  /* print_version will exit.  */
	default:
	  print_usage (true);
	  /* print_usage will exit.  */
	}
    }

  if (bad_accept)
    error ("failed to resolve all accept addresses");

  return optind;
}

int
main (int argc, char *argv[])
{
  const char *p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  progname = p;

  xmalloc_set_program_name (progname);

#ifdef SIGSEGV
  signal (SIGSEGV, crash_signal);
#endif
#ifdef SIGILL
  signal (SIGILL, crash_signal);
#endif
#ifdef SIGBUS
  signal (SIGBUS, crash_signal);
#endif
#ifdef SIGABRT
  signal (SIGABRT, crash_signal);
#endif
#ifdef SIGFPE
  signal (SIGFPE, crash_signal);
#endif
#ifdef SIGPIPE
  /* Ignore sigpipe, so read/write get an error.  */
  signal (SIGPIPE, SIG_IGN);
#endif
#ifdef SIGINT
  signal (SIGINT, kill_signal);
#endif

  int argno = process_args (argc, argv);
  const char *name = NULL;  /* Name of this mapper.  */
  char *cookie = NULL;  /* Cookie to require.  */
  int sock_fd = -1; /* Socket fd, otherwise stdin/stdout.  */
  int err = 0;
  const char *errmsg = NULL;
#ifdef NETWORKING
  int af = AF_UNSPEC;
#endif

  if (argno < argc)
    {
      /* A file of mappings, local or ipv6 address.  */
      const char *option = argv[argno];
      unsigned len = strlen (option);
      char *writable = XNEWVEC (char, len + 1);
      memcpy (writable, option, len + 1);
      cookie = strchr (writable, '?');
      if (cookie)
	{
	  len = cookie - writable;
	  *cookie++ = 0;
	}

      {
	/* Does it look like a socket?  */
#ifdef NETWORKING
#ifdef HAVE_AF_UNIX
	  sockaddr_un un;
	  size_t un_len = 0;
#endif
	  int port = 0;
#ifdef HAVE_AF_INET6
	  struct addrinfo *addrs = NULL;
#endif
#endif
	  if (writable[0] == '=')
	    {
	      /* A local socket.  */
#ifdef HAVE_AF_UNIX
	      if (len < sizeof (un.sun_path))
		{
		  memset (&un, 0, sizeof (un));
		  un.sun_family = AF_UNIX;
		  memcpy (un.sun_path, writable + 1, len);
		}
	      un_len = offsetof (struct sockaddr_un, sun_path) + len + 1;
	      af = AF_UNIX;
#else
	      errmsg = "unix protocol unsupported";
#endif
	      name = writable;
	    }
	  else if (char *colon = (char *)memrchr (writable, ':', len))
	    {
	      /* Try a hostname:port address.  */
	      char *endp;
	      port = strtoul (colon + 1, &endp, 10);
	      if (port && endp != colon + 1 && !*endp)
		{
		  /* Ends in ':number', treat as ipv6 domain socket.  */
#ifdef HAVE_AF_INET6
		  addrinfo hints;

		  hints.ai_flags = AI_NUMERICSERV;
		  hints.ai_family = AF_INET6;
		  hints.ai_socktype = SOCK_STREAM;
		  hints.ai_protocol = 0;
		  hints.ai_addrlen = 0;
		  hints.ai_addr = NULL;
		  hints.ai_canonname = NULL;
		  hints.ai_next = NULL;

		  *colon = 0;
		  /* getaddrinfo requires a port number, but is quite
		     happy to accept invalid ones.  So don't rely on it.  */
		  if (int e = getaddrinfo (colon == writable ? NULL : writable,
					   "0", &hints, &addrs))
		    {
		      err = e;
		      errmsg = "resolving address";
		    }
		  *colon = ':';
		  af = AF_INET6;
#else
		  errmsg = "ipv6 protocol unsupported";
#endif
		  name = writable;
	    }
	}

      if (name)
	{
#ifdef NETWORKING
	  if (af != AF_UNSPEC)
	    {
	      sock_fd = socket (af, SOCK_STREAM, 0);
	      kill_sock_fd = sock_fd;
	    }
#endif
#ifdef HAVE_AF_UNIX
	  if (un_len)
	    if (sock_fd < 0 || bind (sock_fd, (sockaddr *)&un, un_len) < 0)
	      if (sock_fd >= 0)
		{
		  close (sock_fd);
		  sock_fd = -1;
		}
#endif
#ifdef HAVE_AF_INET6
	  if (addrs)
	    {
	      if (sock_fd >= 0)
		{
		  struct addrinfo *next;
		  for (next = addrs; next; next = next->ai_next)
		    if (next->ai_family == af
			&& next->ai_socktype == SOCK_STREAM)
		      {
			sockaddr_in6 *in6 = (sockaddr_in6 *)next->ai_addr;
			in6->sin6_port = htons (port);
			if (ntohs (in6->sin6_port) != port)
			  errno = EINVAL;
			else if (!bind (sock_fd,
					next->ai_addr, next->ai_addrlen))
			  break;
		      }
		  if (!next)
		    {
		      close (sock_fd);
		      sock_fd = -1;
		    }
		  else
		    {
		      if (flag_noisy)
			{
			  const char *str = NULL;
#if HAVE_INET_NTOP
			  char name[INET6_ADDRSTRLEN];
			  sockaddr_in6 *in6 = (sockaddr_in6 *)next->ai_addr;
			  str = inet_ntop (in6->sin6_family, &in6->sin6_addr,
					   name, sizeof (name));
#endif
			  noisy ("binding socket to %s:%d",
				 str ? str : "", port);
			}
		    }
		}
	      freeaddrinfo (addrs);
	    }
#endif
	  kill_sock_fd = sock_fd;
	  if (sock_fd < 0 && !errmsg)
	    {
	      err = errno;
	      errmsg = "binding socket";
	    }
	}
      }

      if (errmsg)
	{
	  errno = err;
	  error ("failed %s of mapper `%s': %s", errmsg, name ? name : option,
		 err < 0 ? gai_strerror (err) : err > 0 ? xstrerror (err)
		 : "Facility not provided");
	}

      if (name)
	argno++;
      else
	XDELETEVEC (writable);
    }

  if (!name)
    /* Use stdin/stdout.  */
    name = "stdin/out";

  if (argno != argc)
    {
      module_map = new module_map_t ();

      for (bool first = true; argno != argc; first = false)
	{
	  const char *option = argv[argno++];
	  unsigned len = strlen (option);
	  char *writable = XNEWVEC (char, len + 1);
	  memcpy (writable, option, len + 1);
	  cookie = strchr (writable, '?');
	  if (cookie)
	    {
	      len = cookie - writable;
	      *cookie++ = 0;
	    }
	  if (FILE *file = fopen (writable, "r"))
	    {
	      read_mapping_file (file, cookie, first);
	      fclose (file);
	    }
	  else
	    error ("failed reading '%s': %s", writable, xstrerror (errno));
	  XDELETEVEC (writable);
	}
    }
  else
    flag_fallback = true;

#ifdef HAVE_AF_INET6
  netmask_set_t::iterator end = netmask_set.end ();
  for (netmask_set_t::iterator iter = netmask_set.begin ();
       iter != end; ++iter)
    {
      netmask_vec_t::iterator e = accept_addrs.end ();
      for (netmask_vec_t::iterator i = accept_addrs.begin (); i != e; ++i)
	if (i->includes (iter->addr))
	  goto present;
      accept_addrs.push_back (*iter);
    present:;
    }
#endif

#ifdef NETWORKING
  if (sock_fd >= 0)
    {
#ifdef HAVE_AF_INET6
      server (af == AF_INET6, sock_fd, cookie);
#else
      server (false, sock_fd, cookie);
#endif
      if (name[0] == '=')
	unlink (&name[1]);
    }
  else
#endif
    {
      gcc_assert (sock_fd < 0);
      singleton (0, 1, cookie);
    }

  return 0;
}
