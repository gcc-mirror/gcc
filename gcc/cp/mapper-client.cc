/* C++ modules.  Experimental!
   Copyright (C) 2017-2023 Free Software Foundation, Inc.
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
#if defined (__unix__)
// Solaris11's socket header used bcopy, which we poison.  cody.hh
// will include it later under the above check
#include <sys/socket.h>
#endif
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#define INCLUDE_MAP
#define INCLUDE_MEMORY
#include "system.h"

#include "line-map.h"
#include "diagnostic-core.h"
#include "mapper-client.h"
#include "intl.h"

#include "../../c++tools/resolver.h"

#if !HOST_HAS_O_CLOEXEC
#define O_CLOEXEC 0
#endif

module_client::module_client (pex_obj *p, int fd_from, int fd_to)
  : Client (fd_from, fd_to), pex (p)
{
}

static module_client *
spawn_mapper_program (char const **errmsg, std::string &name,
		      char const *full_program_name)
{
  /* Split writable at white-space.  No space-containing args for
     you!  */
  // At most every other char could be an argument
  char **argv = new char *[name.size () / 2 + 2];
  unsigned arg_no = 0;
  char *str = new char[name.size ()];
  memcpy (str, name.c_str () + 1, name.size ());

  for (auto ptr = str; ; ++ptr)
    {
      while (*ptr == ' ')
	ptr++;
      if (!*ptr)
	break;

      if (!arg_no)
	{
	  /* @name means look in the compiler's install dir.  */
	  if (ptr[0] == '@')
	    ptr++;
	  else
	    full_program_name = nullptr;
	}

      argv[arg_no++] = ptr;
      while (*ptr && *ptr != ' ')
	ptr++;
      if (!*ptr)
	break;
      *ptr = 0;
    }
  argv[arg_no] = nullptr;

  auto *pex = pex_init (PEX_USE_PIPES, progname, NULL);
  FILE *to = pex_input_pipe (pex, false);
  name = argv[0];
  if (!to)
    *errmsg = "connecting input";
  else
    {
      int flags = PEX_SEARCH;

      if (full_program_name)
	{
	  /* Prepend the invoking path, if the mapper is a simple
	     file name.  */
	  size_t dir_len = progname - full_program_name;
	  std::string argv0;
	  argv0.reserve (dir_len + name.size ());
	  argv0.append (full_program_name, dir_len).append (name);
	  name = std::move (argv0);
	  argv[0] = const_cast <char *> (name.c_str ());
	  flags = 0;
	}
      int err;
      *errmsg = pex_run (pex, flags, argv[0], argv, NULL, NULL, &err);
    }
  delete[] str;
  delete[] argv;

  int fd_from = -1, fd_to = -1;
  if (!*errmsg)
    {
      FILE *from = pex_read_output (pex, false);
      if (from && (fd_to = dup (fileno (to))) >= 0)
	fd_from = fileno (from);
      else
	*errmsg = "connecting output";
      fclose (to);
    }

  if (*errmsg)
    {
      pex_free (pex);
      return nullptr;
    }

  return new module_client (pex, fd_from, fd_to);
}

module_client *
module_client::open_module_client (location_t loc, const char *o,
				   void (*set_repo) (const char *),
				   char const *full_program_name)
{
  module_client *c = nullptr;
  std::string ident;
  std::string name;
  char const *errmsg = nullptr;
  unsigned line = 0;

  if (o && o[0])
    {
      /* Maybe a local or ipv6 address.  */
      name = o;
      auto last = name.find_last_of ('?');
      if (last != name.npos)
	{
	  ident = name.substr (last + 1);
	  name.erase (last);
	}

      if (name.size ())
	{
	  switch (name[0])
	    {
	    case '<':
	      // <from>to or <>fromto, or <>
	      {
		size_t pos = name.find ('>', 1);
		if (pos == std::string::npos)
		  pos = name.size ();
		std::string from (name, 1, pos - 1);
		std::string to;
		if (pos != name.size ())
		  to.append (name, pos + 1, std::string::npos);

		int fd_from = -1, fd_to = -1;
		if (from.empty () && to.empty ())
		  {
		    fd_from = fileno (stdin);
		    fd_to = fileno (stdout);
		  }
		else
		  {
		    char *ptr;
		    if (!from.empty ())
		      {
			/* Sadly str::stoul is not portable.  */
			const char *cstr = from.c_str ();
			fd_from = strtoul (cstr, &ptr, 10);
			if (*ptr)
			  {
			    /* Not a number -- a named pipe.  */
			    int dir = to.empty ()
			      ? O_RDWR | O_CLOEXEC : O_RDONLY | O_CLOEXEC;
			    fd_from = open (cstr, dir);
			  }
			if (to.empty ())
			  fd_to = fd_from;
		      }

		    if (!from.empty () && fd_from < 0)
		      ;
		    else if (to.empty ())
		      ;
		    else
		      {
			const char *cstr = to.c_str ();
			fd_to = strtoul (cstr, &ptr, 10);
			if (*ptr)
			  {
			    /* Not a number, a named pipe.  */
			    int dir = from.empty ()
			      ? O_RDWR | O_CLOEXEC : O_WRONLY | O_CLOEXEC;
			    fd_to = open (cstr, dir);
			    if (fd_to < 0)
			      close (fd_from);
			  }
			if (from.empty ())
			  fd_from = fd_to;
		      }
		  }

		if (fd_from < 0 || fd_to < 0)
		  errmsg = "opening";
		else
		  c = new module_client (fd_from, fd_to);
	      }
	      break;

	    case '=':
	      // =localsocket
	      {
		int fd = -1;
#if CODY_NETWORKING
		fd = Cody::OpenLocal (&errmsg, name.c_str () + 1);
#else
		errmsg = "disabled";
#endif
		if (fd >= 0)
		  c = new module_client (fd, fd);
	      }
	      break;

	    case '|':
	      // |program and args
	      c = spawn_mapper_program (&errmsg, name, full_program_name);
	      break;

	    default:
	      // file or hostname:port
	      {
		auto colon = name.find_last_of (':');
		if (colon != name.npos)
		  {
		    char const *cptr = name.c_str () + colon;
		    char *endp;
		    unsigned port = strtoul (cptr + 1, &endp, 10);

		    if (port && endp != cptr + 1 && !*endp)
		      {
			name[colon] = 0;
			int fd = -1;
#if CODY_NETWORKING
			fd = Cody::OpenInet6 (&errmsg, name.c_str (), port);
#else
			errmsg = "disabled";
#endif
			name[colon] = ':';

			if (fd >= 0)
			  c = new module_client (fd, fd);
		      }
		  }
		
	      }
	      break;
	    }
	}
    }

  if (!c)
    {
      // Make a default in-process client
      bool file = !errmsg && !name.empty ();
      auto r = new module_resolver (!file, true);

      if (file)
	{
	int fd = open (name.c_str (), O_RDONLY | O_CLOEXEC);
	if (fd < 0)
	  errmsg = "opening";
	else
	  {
	    if (int l = r->read_tuple_file (fd, ident, false))
	      {
		if (l > 0)
		  line = l;
		errmsg = "reading";
	      }
	      
	    close (fd);
	  }
	}
      else
	r->set_repo ("gcm.cache");

      auto *s = new Cody::Server (r);
      c = new module_client (s);
    }

#ifdef SIGPIPE
  if (!c->IsDirect ())
    /* We need to ignore sig pipe for a while.  */
    c->sigpipe = signal (SIGPIPE, SIG_IGN);
#endif

  if (errmsg)
    error_at (loc, line ? G_("failed %s mapper %qs line %u")
	      : G_("failed %s mapper %qs"), errmsg, name.c_str (), line);

  // now wave hello!
  c->Cork ();
  c->Connect (std::string ("GCC"), ident);
  c->ModuleRepo ();
  auto packets = c->Uncork ();

  auto &connect = packets[0];
  if (connect.GetCode () == Cody::Client::PC_CONNECT)
    c->flags = Cody::Flags (connect.GetInteger ());
  else if (connect.GetCode () == Cody::Client::PC_ERROR)
    error_at (loc, "failed mapper handshake %s", connect.GetString ().c_str ());

  auto &repo = packets[1];
  if (repo.GetCode () == Cody::Client::PC_PATHNAME)
    set_repo (repo.GetString ().c_str ());

  return c;
}

void
module_client::close_module_client (location_t loc, module_client *mapper)
{
  if (mapper->IsDirect ())
    {
      auto *s = mapper->GetServer ();
      auto *r = s->GetResolver ();
      delete s;
      delete r;
    }
  else
    {
      if (mapper->pex)
	{
	  int fd_write = mapper->GetFDWrite ();
	  if (fd_write >= 0)
	    close (fd_write);

	  int status;
	  pex_get_status (mapper->pex, 1, &status);

	  pex_free (mapper->pex);
	  mapper->pex = NULL;

	  if (WIFSIGNALED (status))
	    error_at (loc, "mapper died by signal %s",
		      strsignal (WTERMSIG (status)));
	  else if (WIFEXITED (status) && WEXITSTATUS (status) != 0)
	    error_at (loc, "mapper exit status %d",
		      WEXITSTATUS (status));
	}
      else
	{
	  int fd_read = mapper->GetFDRead ();
	  close (fd_read);
	}

#ifdef SIGPIPE
      // Restore sigpipe
      if (mapper->sigpipe != SIG_IGN)
	signal (SIGPIPE, mapper->sigpipe);
#endif
    }

  delete mapper;
}
