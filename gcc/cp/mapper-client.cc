/* C++ modules.  Experimental!
   Copyright (C) 2017-2020 Free Software Foundation, Inc.
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

/* Mapper Protocol version.  Very new.  */
#define MAPPER_VERSION 0

#include "config.h"
#include "system.h"

/* Include network stuff first.  Excitingly OSX10.14 uses bcmp here, which
   we poison later!  */
#if defined (HAVE_AF_UNIX) || defined (HAVE_AF_INET6)
/* socket, connect, shutdown  */
# define NETWORKING 1
# include <sys/socket.h>
# ifdef HAVE_AF_UNIX
/* sockaddr_un  */
#  include <sys/un.h>
# endif
# include <netinet/in.h>
# ifdef HAVE_AF_INET6
/* sockaddr_in6, getaddrinfo, freeaddrinfo, gai_strerror, ntohs, htons.  */
#  include <netdb.h>
# endif
#endif
#ifndef HAVE_AF_INET6
# define gai_strerror(X) ""
#endif

#include "line-map.h"
#include "diagnostic-core.h"
#include "mapper-client.h"
#include "intl.h"
#include "coretypes.h"
#include "tree.h"
#include "opts.h"
#include "options.h"
#include "toplev.h"

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


/* Create a mapper.  The mapper may be dead.  Yes, I'm embedding some
   client-side socket handling in the compiler.  At least it's not
   ipv4.  */

char const *
module_mapper::open (location_t loc, const char *option, const char *dflt)
{
  /* We set name as soon as we know what kind of mapper this is.  */
  if (!option)
    option = dflt;

  int err = 0;
  const char *errmsg = NULL;

  /* First copy.  */
  unsigned spaces = 0;
  unsigned len = 0;
  char *cookie = NULL;

  for (; option[len]; len++)
    {
      if (option[len] == ' ')
	spaces++;
      if (option[len] == '?' && !cookie)
	cookie = const_cast<char *> (&option[len]);
    }
  char *writable = XNEWVEC (char, len + 1);
  memcpy (writable, option, len + 1);
  if (cookie)
    {
      len = cookie - option;
      cookie = writable + len;
      *cookie = 0;
    }

  if (writable[0] == '|')
    {
      /* A program to spawn and talk to.  */
      /* Split writable at white-space.  No space-containing args
	 for you!  */
      char **argv = XALLOCAVEC (char *, spaces + 2);
      unsigned arg_no = 0;

      for (char *ptr = writable + 1; ; ptr++)
	{
	  argv[arg_no] = ptr;
	  for (;; ptr++)
	    {
	      if (*ptr == ' ')
		break;
	      else if (*ptr)
		continue;
	      else if (ptr != cookie)
		break;
	      else if (arg_no != 1)
		{
		  /* Not a cookie after all.  */
		  *cookie = '?';
		  cookie = NULL;
		}
	    }
	  if (!arg_no++)
	    len = ptr - (writable + 1);	  
	  if (!*ptr)
	    break;
	  *ptr = 0;
	}
      argv[arg_no] = NULL;

      pex = pex_init (PEX_USE_PIPES, progname, NULL);
      FILE *to = pex_input_pipe (pex, false);
      if (!to)
	{
	  err = errno;
	  errmsg = "connecting input";
	}
      else
	{
	  int flags = PEX_SEARCH;

	  /* Use strcmp to detect default, so we may explicitly name
	     it with additional args in tests etc.  */
	  if ((option == dflt || 0 == strcmp (argv[0], dflt + 1))
	      && save_decoded_options[0].opt_index == OPT_SPECIAL_program_name
	      && save_decoded_options[0].arg != progname)
	    {
	      /* Prepend the invoking path.  */
	      const char *fullname = save_decoded_options[0].arg;
	      size_t dir_len = progname - fullname;
	      char *argv0 = XNEWVEC (char, dir_len + len + 1);
	      memcpy (argv0, fullname, dir_len);
	      memcpy (argv0 + dir_len, argv[0], len + 1);
	      argv[0] = argv0;
	      flags = 0;
	    }
	  errmsg = pex_run (pex, flags, argv[0], argv, NULL, NULL, &err);
	  if (!flags)
	    XDELETEVEC (argv[0]);
	}

      if (!errmsg)
	{
	  from = pex_read_output (pex, false);
	  if (from
	      && (fd_to = dup (fileno (to))) >= 0)
	    fd_from = fileno (from);
	  else
	    {
	      err = errno;
	      errmsg = "connecting output";
	    }
	  fclose (to);
	}
      name = writable;
    }
  else if (writable[0] == '<')
    {
      /* File descriptors, inc stdin/out.  */
      int from = -1, to = -1;
      char *ptr = writable + 1, *eptr;
      from = strtoul (ptr, &eptr, 0);
      if (*eptr == '>')
	{
	  ptr = eptr + 1;
	  to = strtoul (ptr, &eptr, 0);
	  if (eptr != ptr && from == -1)
	    from = to;
	}
      if (*eptr)
	errmsg = "parsing";
      else
	{
	  if (eptr == writable + 2)
	    {
	      from = fileno (stdin);
	      to = fileno (stdout);
	    }
	  fd_to = to;
	  fd_from = from;
	}
      name = writable;
    }

  if (!name)
    {
      int fd;

      /* Does it look like a socket?  */
#ifdef NETWORKING
#ifdef HAVE_AF_UNIX
      sockaddr_un un;
      size_t un_len = 0;
      un.sun_family = AF_UNSPEC;
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
	      else
		un.sun_family = AF_INET6;
	      *colon = ':';
#else
	      errmsg = "ipv6 protocol unsupported";
#endif
	      name = writable;
	    }
	}
      
      if (un.sun_family != AF_UNSPEC)
	{
	  fd = socket (un.sun_family, SOCK_STREAM, 0);
	  if (fd < 0)
	    ;
#ifdef HAVE_AF_UNIX
	  else if (un.sun_family == AF_UNIX)
	    {
	      if (connect (fd, (sockaddr *)&un, un_len) < 0)
		{
		  ::close (fd);
		  fd = -1;
		}
	    }
#endif
#ifdef HAVE_AF_INET6
	  else if (un.sun_family == AF_INET6)
	    {
	      struct addrinfo *next;
	      for (next = addrs; next; next = next->ai_next)
		if (next->ai_family == AF_INET6
		    && next->ai_socktype == SOCK_STREAM)
		  {
		    sockaddr_in6 *in6 = (sockaddr_in6 *)next->ai_addr;
		    in6->sin6_port = htons (port);
		    if (ntohs (in6->sin6_port) != port)
		      errno = EINVAL;
		    else if (!connect (fd, next->ai_addr, next->ai_addrlen))
		      break;
		  }

	      if (!next)
		{
		  ::close (fd);
		  fd = -1;
		}
	    }
#endif
	  else
	    gcc_unreachable ();

#ifdef HAVE_AF_INET6
	  freeaddrinfo (addrs);
#endif
	  if (fd >= 0)
	    {
	      /* We have a socket.  */
	      fd_from = fd_to = fd;
#ifdef SIGPIPE
	      /* We need to ignore sig pipe for a while.  */
	      sigpipe = signal (SIGPIPE, SIG_IGN);
#endif
	    }
	  else if (!errmsg)
	    {
	      err = errno;
	      errmsg = "connecting socket";
	    }
	}
    }

  if (!name)
    {
      /* Try a mapping file.  */
      from = fopen (writable, "r");
      if (from)
	fd_from = fileno (from);
      else
	{
	  err = errno;
	  errmsg = "opening";
	}
      name = writable;
    }

  if (errmsg)
    {
      errno = err;
      error_at (loc, err <= 0 ? G_("failed %s of mapper %qs: %s")
		: G_("failed %s of mapper %qs: %m"),
		errmsg, name ? name : option,
		err < 0 ? gai_strerror (err) : _("Facility not provided"));
      close (loc);
      goto done;
    }

  /* Exercise buffer expansion code.  */
  size = !MAPPER_VERSION ? 3 : 200;
  pos = end = buffer = XNEWVEC (char, size);

 done:

  /* Return any cookie we found.  */
  return cookie ? cookie + 1 : cookie;
}

void
module_mapper::close (location_t loc)
{
  if (fd_to >= 0)
    {
#ifdef NETWORKING
      if (fd_to == fd_from)
	{
	  shutdown (fd_to, SHUT_WR);
#ifdef SIGPIPE
	  if (sigpipe != SIG_IGN)
	    /* Restore sigpipe.  */
	    signal (SIGPIPE, sigpipe);
#endif
	}
      else
#endif
	::close (fd_to);
      fd_to = -1;
    }
  

  if (pex)
    {
      int status;
      pex_get_status (pex, 1, &status);

      pex_free (pex);
      pex = NULL;

      if (WIFSIGNALED (status))
	error_at (loc, "mapper %qs died by signal %s",
		  name, strsignal (WTERMSIG (status)));
      else if (WIFEXITED (status) && WEXITSTATUS (status) != 0)
	error_at (loc, "mapper %qs exit status %d",
		  name, WEXITSTATUS (status));
      from = NULL;
      fd_from = -1;
    }
  else
    {
      if (fd_from >= 0)
	{
	  if (!is_file ())
	    ::close (fd_from);
	  fd_from = -1;
	}
    }

  XDELETEVEC (buffer);
  buffer = pos = end = NULL;
  size = 0;
}

/* Send a command to the mapper.  */

void
module_mapper::send_command (location_t loc, const char *format, ...)
{
  size_t actual = 0;
  if (pos != buffer)
    pos = end = buffer;
  if (batching)
    *end++ = '+';
  else if (end != buffer)
    *end++ = '-';

  for (;;)
    {
      va_list args;
      va_start (args, format);
      size_t available = (buffer + size) - end;
      actual = vsnprintf (end, available, format, args);
      va_end (args);
      if (actual < available)
	break;

      size = size * 2 + actual + 20;
      char *next = XRESIZEVEC (char, buffer, size);
      end = next + (end - buffer);
      buffer = pos = next;
    }

  end += actual;
  *end++ = '\n';
  if (!batching)
    {
      if (is_live () && end - buffer != write (fd_to, buffer, end - buffer))
	error_at (loc, "failed write to mapper %qs: %m", name);
      end = pos = buffer;
    }
}

/* Read a response from the mapper.  -ve -> end, 0 -> blank, +ve -> something*/

int
module_mapper::get_response (location_t loc)
{
  if (batching)
    pos = end + 1;
  else
    {
      gcc_assert (pos == end);
      size_t off = 0;
      bool bol = true;
      bool last = false;
      int stop = 0;

      if (is_live ())
	{
	  for (;;)
	    {
	      if (fd_to < 0)
		{
		  /* We're reading a file.  There can be no
		     continuations.  */
		  if (!fgets (buffer + off, size - off, from))
		    {
		      stop = feof (from) ? +1 : -1;
		      break;
		    }
		  off += strlen (buffer + off);
		  if (off && buffer[off - 1] == '\n')
		    break;
		}
	      else
		{
		  /* Reading a pipe or socket.  */
		  int bytes = read (fd_from, buffer + off, size - off - 1);
		  if (bytes <= 0)
		    {
		      stop = bytes ? -1 : +1;
		      break;
		    }
		  while (bytes)
		    {
		      if (bol)
			{
			  if (buffer[off] == '+')
			    batching = true;
			  else
			    last = true;
			}
		      bol = false;
		      if (char *eol
			  = (char *)memchr (buffer + off, '\n', size - off))
			{
			  bol = true;
			  unsigned nline = eol + 1 - buffer;
			  bytes -= nline - off;
			  off = nline;
			}
		      else
			{
			  off += bytes;
			  bytes = 0;
			  break;
			}
		    }
		  if (bol && last)
		    break;
		}
	      if (off + 1 == size)
		{
		  size *= 2;
		  buffer = XRESIZEVEC (char, buffer, size);
		}
	    }

	  if (stop)
	    {
	      if (stop < 0)
		error_at (loc, "failed read of mapper %qs: %m", name);
	      else if (is_server ())
		error_at (loc, "unexpected close from mapper %qs", name);
	      start = NULL;
	      return -1;
	    }

	  off--;
	}

      buffer[off] = 0;
      end = buffer + off;
      pos = buffer;
    }

  for (;; pos = end + 1)
    {
      start = pos;
      end = NULL;
      if (*pos == '+')
	{
	  pos++;
	  end = strchr (pos, '\n');
	  if (end)
	    *end = 0;
	}

      if (!end)
	{
	  if (*pos == '-')
	    pos++;
	  end = pos + strlen (pos);
	  batching = false;
	}

      while (*pos && ISSPACE (*pos))
	pos++;

      if (*pos)
	return true;
      if (!batching)
	break;
    }

  return false;
}

void
module_mapper::response_unexpected (location_t loc)
{
  if (start)
    {
      /* Restore the whitespace we zapped tokenizing.  */
      for (char *ptr = start; ptr != pos; ptr++)
	if (!*ptr)
	  *ptr = ' ';
      error_at (loc, "mapper response malformed: %qs", start);
    }
  pos = end;
}

bool
module_mapper::response_eol (location_t loc, bool ignore)
{
  bool at_end = eol_p ();
  if (!at_end && !ignore)
    response_unexpected (loc);
  pos = end;
  return at_end;
}

char *
module_mapper::response_token (location_t loc, bool all)
{
  char *ptr = pos;

  if (ptr == end)
    {
      response_unexpected (loc);
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

int
module_mapper::response_word (location_t loc, const char *option, ...)
{
  if (const char *tok = response_token (loc))
    {
      va_list args;
      int count = 0;

      va_start (args, option);
      do
	{
	  if (!strcmp (option, tok))
	    {
	      va_end (args);
	      return count;
	    }
	  count++;
	  option = va_arg (args, const char *);
	}
      while (option);
      va_end (args);
      response_unexpected (loc);
    }
  return -1;
}

/*  Module mapper protocol non-canonical precis:

    HELLO version kind cookie
    	-> HELLO/ERROR response
    IMPORT module-name
    	-> OK bmipath
	-> ERROR
    EXPORT module-name
    	-> OK bmipath
    DONE module-name
    	No response
    RESET
        No response
 */

/* Start handshake.  */

bool
module_mapper::handshake (location_t loc, const char *cookie,
			  char **repo)
{
  send_command (loc, "HELLO %d GCC %s", MAPPER_VERSION, cookie);

  bool ok = get_response (loc) > 0;
  switch (response_word (loc, "HELLO", "ERROR", NULL))
    {
    default:
      ok = false;
      break;

    case 0: /* HELLO $ver $agent $repo */
      {
	ATTRIBUTE_UNUSED const char *ver = response_token (loc);
	ATTRIBUTE_UNUSED const char *agent
	  = !eol_p () ? response_token (loc) : NULL;

	/* Notice this is pointing into the buffer, so only lives
	   until the next message.  */
	if (!eol_p ())
	  *repo = response_token (loc, true);

	if (response_eol (loc))
	  ok = true;
      }
      break;

    case 1: /* ERROR $msg */
      error_at (loc, "mapper handshake failure: %s", response_error ());
      ok = false;
      break;
    }

  return ok;
}

/* IMPORT or EXPORT query.  */

void
module_mapper::imex_query (location_t loc, const char *flatname, bool exporting)
{
  send_command (loc, "%sPORT %s", exporting ? "EX" : "IM", flatname);
}

const char *
module_mapper::imex_response (location_t loc, const char *flatname)
{
  return get_response (loc) > 0 ? cmi_response (loc, flatname) : NULL;
}

/* Response to import/export query.  */

const char *
module_mapper::cmi_response (location_t loc, const char *flatname)
{
  char *filename = NULL;

  switch (response_word (loc, "OK", "ERROR", NULL))
    {
    default:
      break;

    case 0: /* OK $cmifile  */
      filename = response_token (loc, true);
      response_eol (loc);
      break;

    case 1: /* ERROR $msg */
      error_at (loc, "mapper cannot provide module %qs: %s",
		flatname, response_error ());
      break;
    }

  return filename;
}

/* Include translation.  Query if PATH should be turned into a header
   import.  Return false if it should remain a #include, true
   otherwise.  */

bool
module_mapper::translate_include (location_t loc, const char *path)
{
  gcc_assert (is_server ());
  bool xlate = false;

  send_command (loc, "INCLUDE %s", path);
  if (get_response (loc) <= 0)
    return false;

  switch (response_word (loc, "IMPORT", "TEXT", NULL))
    {
    default:
      break;

    case 0:  /* Divert to import.  */
      xlate = true;
      break;

    case 1:  /* Treat as include.  */
      break;
    }
  response_eol (loc);

  return xlate;
}
