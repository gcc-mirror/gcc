/* C++ modules.  Experimental!
   Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

#include "config.h"
#include "resolver.h"

// C++
#include <set>
#include <vector>
#include <map>
// C
#include <csignal>
#include <cstring>
#include <cstdarg>
#include <cstdlib>
// OS
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// Network
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

#ifndef AI_NUMERICSERV
#define AI_NUMERICSERV 0
#endif

#include <getopt.h>

// Select or epoll
#if NETWORKING
#ifdef HAVE_EPOLL
/* epoll_create, epoll_ctl, epoll_pwait  */
#include <sys/epoll.h>
#endif
#if defined (HAVE_PSELECT) || defined (HAVE_SELECT)
/* pselect or select  */
#include <sys/select.h>
#endif
#endif

// GCC
#include "version.h"
#include "ansidecl.h"
#define HAVE_DECL_BASENAME 1 /* See comment in gcc/configure.ac.  */
#include "libiberty.h"

#if !HOST_HAS_O_CLOEXEC
#define O_CLOEXEC 0
#endif

#ifndef IS_DIR_SEPARATOR
#define IS_DIR_SEPARATOR(C) ((C) == '/')
#endif
#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

/* Imported from libcpp/system.h
   Use gcc_assert(EXPR) to test invariants.  */
#if ENABLE_ASSERT_CHECKING
#define gcc_assert(EXPR)                                                \
   ((void)(!(EXPR) ? fancy_abort (__FILE__, __LINE__, __FUNCTION__), 0 : 0))
#elif (GCC_VERSION >= 4005)
#define gcc_assert(EXPR)                                                \
  ((void)(__builtin_expect (!(EXPR), 0) ? __builtin_unreachable (), 0 : 0))
#else
/* Include EXPR, so that unused variable warnings do not occur.  */
#define gcc_assert(EXPR) ((void)(0 && (EXPR)))
#endif

/* Use gcc_unreachable() to mark unreachable locations (like an
   unreachable default case of a switch.  Do not use gcc_assert(0).  */
#if (GCC_VERSION >= 4005) && !ENABLE_ASSERT_CHECKING
#define gcc_unreachable() __builtin_unreachable ()
#else
#define gcc_unreachable() (fancy_abort (__FILE__, __LINE__, __FUNCTION__))
#endif


#if NETWORKING
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
#endif

const char *progname;

/* Speak thoughts out loud.  */
static bool flag_noisy = false;

/* One and done.  */
static bool flag_one = false;

/* Serialize connections.  */
static bool flag_sequential = false;

/* Fallback to default if map file is unrewarding.  */
static bool flag_map = false;

/* Fallback to xlate if map file is unrewarding.  */
static bool flag_xlate = false;

/* Root binary directory.  */
static const char *flag_root = "gcm.cache";

#if NETWORKING
static netmask_set_t netmask_set;

static netmask_vec_t accept_addrs;
#endif

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

  exit (2);
}

/* Hooked to from gcc_assert & gcc_unreachable.  */

#if ENABLE_ASSERT_CHECKING
void ATTRIBUTE_NORETURN ATTRIBUTE_COLD
fancy_abort (const char *file, int line, const char *func)
{
  internal_error ("in %s, at %s:%d", func, trim_src_file (file), line);
}
#endif

/* Exploded on a signal.  */

static void ATTRIBUTE_NORETURN ATTRIBUTE_COLD
crash_signal (int sig)
{
  signal (sig, SIG_DFL);
  // strsignal is not portable :(
  internal_error ("signal %d", sig);
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

#if NETWORKING
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
#endif

/* More messages to the user.  */

static void ATTRIBUTE_PRINTF_2
fnotice (FILE *file, const char *fmt, ...)
{
  va_list args;

  va_start (args, fmt);
  vfprintf (file, fmt, args);
  va_end (args);
}

static void ATTRIBUTE_NORETURN
print_usage (int error_p)
{
  FILE *file = error_p ? stderr : stdout;
  int status = error_p ? 1 : 0;

  fnotice (file, "Usage: %s [OPTION...] [CONNECTION] [MAPPINGS...] \n\n",
	   progname);
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
  fnotice (stdout, "%s %s%s\n", progname, pkgversion_string, version_string);
  fprintf (stdout, "Copyright %s 2018-2024 Free Software Foundation, Inc.\n",
	   ("(C)"));
  fnotice (stdout,
	   ("This is free software; see the source for copying conditions.\n"
	    "There is NO warranty; not even for MERCHANTABILITY or \n"
	    "FITNESS FOR A PARTICULAR PURPOSE.\n\n"));
  exit (0);
}

/* ARG is a netmask to accept from.  Add it to the table.  Return
   false if we fail to resolve it.  */

static bool
accept_from (char *arg ATTRIBUTE_UNUSED)
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
  /* getaddrinfo requires either hostname or servname to be non-null, so that we must
     set a port number (to cover the case that the string passed contains just /NN).
     Use an arbitrary in-range port number, but avoiding "0" which triggers a bug on
     some BSD variants.  */
  if (int e = getaddrinfo (slash == arg ? NULL : arg, "1", &hints, &addrs))
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
     { "help",	no_argument,	NULL, 'h' },
     { "map",   no_argument,	NULL, 'm' },
     { "noisy",	no_argument,	NULL, 'n' },
     { "one",	no_argument,	NULL, '1' },
     { "root",	required_argument, NULL, 'r' },
     { "sequential", no_argument, NULL, 's' },
     { "translate",no_argument,	NULL, 't' },
     { "version", no_argument,	NULL, 'v' },
     { 0, 0, 0, 0 }
    };
  int opt;
  bool bad_accept = false;
  const char *opts = "a:fhmn1r:stv";
  while ((opt = getopt_long (argc, argv, opts, options, NULL)) != -1)
    {
      switch (opt)
	{
	case 'a':
	  if (!accept_from (optarg))
	    bad_accept = true;
	  break;
	case 'h':
	  print_usage (false);
	  /* print_usage will exit.  */
	case 'f': // deprecated alias
	case 'm':
	  flag_map = true;
	  break;
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
	case 't':
	  flag_xlate = true;
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

#if NETWORKING

/* Manipulate the EPOLL state, or do nothing, if there is epoll.  */

#ifdef HAVE_EPOLL
static inline void
do_epoll_ctl (int epoll_fd, int code, int event, int fd, unsigned data)
{
  epoll_event ev;
  ev.events = event;
  ev.data.u32 = data;
  if (epoll_ctl (epoll_fd, code, fd, &ev))
    {
      noisy ("epoll_ctl error:%s", xstrerror (errno));
      gcc_unreachable ();
    }
}
#define my_epoll_ctl(EFD,C,EV,FD,CL) \
  ((EFD) >= 0 ? do_epoll_ctl (EFD,C,EV,FD,CL) : (void)0)
#else
#define my_epoll_ctl(EFD,C,EV,FD,CL) ((void)(EFD), (void)(FD), (void)(CL))
#endif

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

bool process_server (Cody::Server *server, unsigned slot, int epoll_fd)
{
  switch (server->GetDirection ())
    {
    case Cody::Server::READING:
      if (int err = server->Read ())
	return !(err == EINTR || err == EAGAIN);
      server->ProcessRequests ();
      server->PrepareToWrite ();
      break;

    case Cody::Server::WRITING:
      if (int err = server->Write ())
	return !(err == EINTR || err == EAGAIN);
      server->PrepareToRead ();
      break;

    default:
      // We should never get here
      return true;
    }

  // We've changed direction, so update epoll
  gcc_assert (server->GetFDRead () == server->GetFDWrite ());
  my_epoll_ctl (epoll_fd, EPOLL_CTL_MOD,
		server->GetDirection () == Cody::Server::READING
		? EPOLLIN : EPOLLOUT, server->GetFDRead (), slot + 1);

  return false;
}

void close_server (Cody::Server *server, int epoll_fd)
{
  my_epoll_ctl (epoll_fd, EPOLL_CTL_DEL, EPOLLIN, server->GetFDRead (), 0);

  close (server->GetFDRead ());
  
  delete server;
}

int open_server (bool ip6, int sock_fd)
{
  sockaddr_in6 addr;
  socklen_t addr_len = sizeof (addr);

#ifdef HAVE_ACCEPT4
  int client_fd = accept4 (sock_fd, ip6 ? (sockaddr *)&addr : nullptr,
			   &addr_len, SOCK_NONBLOCK);
#else
  int client_fd = accept (sock_fd, ip6 ? (sockaddr *)&addr : nullptr, &addr_len);
#endif
  if (client_fd < 0)
    {
      error ("cannot accept: %s", xstrerror (errno));
      flag_one = true;
    }
  else if (ip6)
    {
      const char *str = NULL;
#if HAVE_INET_NTOP
      char name[INET6_ADDRSTRLEN];
      str = inet_ntop (addr.sin6_family, &addr.sin6_addr, name, sizeof (name));
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
	present:;
	}
      if (client_fd >= 0)
	flag_noisy && noisy ("Accepting connection from '%s'", str ? str : "");
    }

  return client_fd;
}

/* A server listening on bound socket SOCK_FD.  */

static void
server (bool ipv6, int sock_fd, module_resolver *resolver)
{
  int epoll_fd = -1;

  signal (SIGTERM, term_signal);
#ifdef HAVE_EPOLL
  epoll_fd = epoll_create (1);
#endif
  if (epoll_fd >= 0)
    my_epoll_ctl (epoll_fd, EPOLL_CTL_ADD, EPOLLIN, sock_fd, 0);

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
#endif
  if (term_pipe)
    pipe (term_pipe);

  // We need stable references to servers, so this array can contain nulls
  std::vector<Cody::Server *> connections;
  unsigned live = 0;
  while (sock_fd >= 0 || live)
    {
      /* Wait for one or more events.  */
      bool eintr = false;
      int event_count;

      if (epoll_fd >= 0)
	{
#ifdef HAVE_EPOLL
	  event_count = epoll_pwait (epoll_fd, events, max_events, -1, &mask);
#endif
	}
      else
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

	  for (auto iter = connections.begin ();
	       iter != connections.end (); ++iter)
	    if (auto *server = *iter)
	      {
		int fd = -1;
		switch (server->GetDirection ())
		  {
		  case Cody::Server::READING:
		    fd = server->GetFDRead ();
		    FD_SET (fd, &readers);
		    break;
		  case Cody::Server::WRITING:
		    fd = server->GetFDWrite ();
		    FD_SET (fd, &writers);
		    break;
		  default:
		    break;
		  }

		if (fd >= 0 && limit <= unsigned (fd))
		  limit = fd + 1;
	      }

#ifdef HAVE_PSELECT
	  event_count = pselect (limit, &readers, &writers, NULL, NULL, &mask);
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
	  // Error in waiting
	  if (errno == EINTR)
	    {
	      flag_noisy && noisy ("Interrupted wait");
	      eintr = true;
	    }
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

      auto iter = connections.begin ();
      while (event_count--)
	{
	  // Process an event
	  int active = -2;

	  if (epoll_fd >= 0)
	    {
#ifdef HAVE_EPOLL
	      /* See PR c++/88664 for why a temporary is used.  */
	      unsigned data = events[event_count].data.u32;
	      active = int (data) - 1;
#endif
	    }
	  else
	    {
	      for (; iter != connections.end (); ++iter)
		if (auto *server = *iter)
		  {
		    bool found = false;
		    switch (server->GetDirection ())
		      {
#if defined (HAVE_PSELECT) || defined (HAVE_SELECT)
		      case Cody::Server::READING:
			found = FD_ISSET (server->GetFDRead (), &readers);
			break;
		      case Cody::Server::WRITING:
			found = FD_ISSET (server->GetFDWrite (), &writers);
			break;
#endif
		      default:
			break;
		      }

		    if (found)
		      {
			active = iter - connections.begin ();
			++iter;
			break;
		      }
		  }

#if defined (HAVE_PSELECT) || defined (HAVE_SELECT)
	      if (active < 0 && sock_fd >= 0 && FD_ISSET (sock_fd, &readers))
		active = -1;
#endif
	    }

	  if (active >= 0)
	    {
	      // Do the action
	      auto *server = connections[active];
	      if (process_server (server, active, epoll_fd))
		{
		  connections[active] = nullptr;
		  close_server (server, epoll_fd);
		  live--;
		  if (flag_sequential)
		    my_epoll_ctl (epoll_fd, EPOLL_CTL_ADD, EPOLLIN, sock_fd, 0);
		}
	    }
	  else if (active == -1 && !eintr)
	    {
	      // New connection
	      int fd = open_server (ipv6, sock_fd);
	      if (fd >= 0)
		{
#if !defined (HAVE_ACCEPT4) \
  && (defined (HAVE_EPOLL) || defined (HAVE_PSELECT) || defined (HAVE_SELECT))
		  int flags = fcntl (fd, F_GETFL, 0);
		  fcntl (fd, F_SETFL, flags | O_NONBLOCK);
#endif
		  auto *server = new Cody::Server (resolver, fd);

		  unsigned slot = connections.size ();
		  if (live == slot)
		    connections.push_back (server);
		  else
		    for (auto iter = connections.begin (); ; ++iter)
		      if (!*iter)
			{
			  *iter = server;
			  slot = iter - connections.begin ();
			  break;
			}
		  live++;
		  my_epoll_ctl (epoll_fd, EPOLL_CTL_ADD, EPOLLIN, fd, slot + 1);
		}
	    }

	  if (sock_fd >= 0
	      && (term || (live && (flag_one || flag_sequential))))
	    {
	      /* Stop paying attention to sock_fd.  */
	      my_epoll_ctl (epoll_fd, EPOLL_CTL_DEL, EPOLLIN, sock_fd, 0);
	      if (flag_one || term)
		{
		  close (sock_fd);
		  sock_fd = -1;
		}
	    }
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

static int maybe_parse_socket (std::string &option, module_resolver *r)
{
  /* Local or ipv6 address.  */
  auto last = option.find_last_of ('?');
  if (last != option.npos)
    {
      r->set_ident (option.c_str () + last + 1);
      option.erase (last);
    }
  int fd = -2;
  char const *errmsg = nullptr;

  /* Does it look like a socket?  */
  if (option[0] == '=')
    {
      /* A local socket.  */
#if CODY_NETWORKING
      fd = Cody::ListenLocal (&errmsg, option.c_str () + 1);
#endif
    }
  else
    {
      auto colon = option.find_last_of (':');
      if (colon != option.npos)
	{
	  /* Try a hostname:port address.  */
	  char const *cptr = option.c_str () + colon;
	  char *endp;
	  unsigned port = strtoul (cptr + 1, &endp, 10);

	  if (port && endp != cptr + 1 && !*endp)
	    {
	      /* Ends in ':number', treat as ipv6 domain socket.  */
	      option.erase (colon);
#if CODY_NETWORKING
	      fd = Cody::ListenInet6 (&errmsg, option.c_str (), port);
#endif
	    }
	}
    }

  if (errmsg)
    error ("failed to open socket: %s", errmsg);

  return fd;
}

int
main (int argc, char *argv[])
{
  const char *p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  progname = p;

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
#if NETWORKING
#ifdef SIGINT
  signal (SIGINT, kill_signal);
#endif
#endif

  int argno = process_args (argc, argv);

  std::string name;
  int sock_fd = -1; /* Socket fd, otherwise stdin/stdout.  */
  module_resolver r (flag_map, flag_xlate);

  if (argno != argc)
    {
      name = argv[argno];
      sock_fd = maybe_parse_socket (name, &r);
      if (!name.empty ())
	argno++;
    }

  if (argno != argc)
    for (; argno != argc; argno++)
      {
	std::string option = argv[argno];
	char const *prefix = nullptr;
	auto ident = option.find_last_of ('?');
	if (ident != option.npos)
	  {
	    prefix = option.c_str () + ident + 1;
	    option[ident] = 0;
	  }
	int fd = open (option.c_str (), O_RDONLY | O_CLOEXEC);
	int err = 0;
	if (fd < 0)
	  err = errno;
	else
	  {
	    err = r.read_tuple_file (fd, prefix, false);
	    close (fd);
	  }

	if (err)
	  error ("failed reading '%s': %s", option.c_str (), xstrerror (err));
      }
  else
    r.set_default_map (true);

  if (flag_root)
    r.set_repo (flag_root);

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

#if NETWORKING
  if (sock_fd >= 0)
    {
      server (name[0] != '=', sock_fd, &r);
      if (name[0] == '=')
	unlink (name.c_str () + 1);
    }
  else
#endif
    {
      auto server = Cody::Server (&r, 0, 1);

      int err = 0;
      for (;;)
	{
	  server.PrepareToRead ();
	  while ((err = server.Read ()))
	    {
	      if (err == EINTR || err == EAGAIN)
		continue;
	      goto done;
	    }

	  server.ProcessRequests ();

	  server.PrepareToWrite ();
	  while ((err = server.Write ()))
	    {
	      if (err == EINTR || err == EAGAIN)
		continue;
	      goto done;
	    }
	}
    done:;
      if (err > 0)
	error ("communication error:%s", xstrerror (err));
    }

  return 0;
}
