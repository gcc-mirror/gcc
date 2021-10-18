/* C++ modules.  Experimental!	-*- c++ -*-
   Copyright (C) 2017-2021 Free Software Foundation, Inc.
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
#include <algorithm>
// C
#include <cstring>
// OS
#include <fcntl.h>
#include <unistd.h>
#if 0 // 1 for testing no mmap
#define MAPPED_READING 0
#else
#ifdef IN_GCC
#if HAVE_MMAP_FILE && _POSIX_MAPPED_FILES > 0
#define MAPPED_READING 1
#else
#define MAPPED_READING 0
#endif
#else
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#define MAPPED_READING 1
#else
#define MAPPED_READING 0
#endif
#endif
#endif

#include <sys/types.h>
#include <sys/stat.h>

#if !defined (IN_GCC) && !MAPPED_READING
#define xmalloc(X) malloc(X)
#endif

#if !HOST_HAS_O_CLOEXEC
#define O_CLOEXEC 0
#endif

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

module_resolver::module_resolver (bool map, bool xlate)
  : default_map (map), default_translate (xlate)
{
}

module_resolver::~module_resolver ()
{
  if (fd_repo >= 0)
    close (fd_repo);
}

bool
module_resolver::set_repo (std::string &&r, bool force)
{
  if (force || repo.empty ())
    {
      repo = std::move (r);
      force = true;
    }
  return force;
}

bool
module_resolver::add_mapping (std::string &&module, std::string &&file,
			      bool force)
{
  auto res = map.emplace (std::move (module), std::move (file));
  if (res.second)
    force = true;
  else if (force)
    res.first->second = std::move (file);

  return force;
}

int
module_resolver::read_tuple_file (int fd, char const *prefix, bool force)
{
  struct stat stat;
  if (fstat (fd, &stat) < 0)
    return -errno;

  if (!stat.st_size)
    return 0;

  void *buffer = nullptr;
#if MAPPED_READING
  // Just map the file, we're gonna read all of it, so no need for
  // line buffering
  buffer = mmap (nullptr, stat.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
  if (buffer == MAP_FAILED)
    return -errno;
#else
  buffer = xmalloc (stat.st_size);
  if (!buffer)
    return -errno;
  if (read (fd, buffer, stat.st_size) != stat.st_size)
    return -errno;
#endif

  size_t prefix_len = prefix ? strlen (prefix) : 0;
  unsigned lineno = 0;

  for (char const *begin = reinterpret_cast <char const *> (buffer),
	 *end = begin + stat.st_size, *eol;
       begin != end; begin = eol + 1)
    {
      lineno++;
      eol = std::find (begin, end, '\n');
      if (eol == end)
	// last line has no \n, ignore the line, you lose
	break;

      auto *pos = begin;
      bool pfx_search = prefix_len != 0;

    pfx_search:
      while (*pos == ' ' || *pos == '\t')
	pos++;

      auto *space = pos;
      while (*space != '\n' && *space != ' ' && *space != '\t')
	space++;

      if (pos == space)
	// at end of line, nothing here	
	continue;

      if (pfx_search)
	{
	  if (size_t (space - pos) == prefix_len
	      && std::equal (pos, space, prefix))
	    pfx_search = false;
	  pos = space;
	  goto pfx_search;
	}

      std::string module (pos, space);
      while (*space == ' ' || *space == '\t')
	space++;
      std::string file (space, eol);

      if (module[0] == '$')
	{
	  if (module == "$root")
	    set_repo (std::move (file));
	  else
	    return lineno;
	}
      else
	{
	  if (file.empty ())
	    file = GetCMIName (module);
	  add_mapping (std::move (module), std::move (file), force);
	}
    }

#if MAPPED_READING
  munmap (buffer, stat.st_size);
#else
  free (buffer);
#endif

  return 0;
}

char const *
module_resolver::GetCMISuffix ()
{
  return "gcm";
}

module_resolver *
module_resolver::ConnectRequest (Cody::Server *s, unsigned version,
				 std::string &a, std::string &i)
{
  if (!version || version > Cody::Version)
    s->ErrorResponse ("version mismatch");
  else if (a != "GCC")
    // Refuse anything but GCC
    ErrorResponse (s, std::string ("only GCC supported"));
  else if (!ident.empty () && ident != i)
    // Failed ident check
    ErrorResponse (s, std::string ("bad ident"));
  else
    // Success!
    s->ConnectResponse ("gcc");

  return this;
}

int
module_resolver::ModuleRepoRequest (Cody::Server *s)
{
  s->PathnameResponse (repo);
  return 0;
}

int
module_resolver::cmi_response (Cody::Server *s, std::string &module)
{
  auto iter = map.find (module);
  if (iter == map.end ())
    {
      std::string file = default_map ? GetCMIName (module) : std::string ();
      auto res = map.emplace (module, file);
      iter = res.first;
    }

  if (iter->second.empty ())
    s->ErrorResponse ("no such module");
  else
    s->PathnameResponse (iter->second);

  return 0;
}

int
module_resolver::ModuleExportRequest (Cody::Server *s, Cody::Flags,
				      std::string &module)
{
  return cmi_response (s, module);
}

int
module_resolver::ModuleImportRequest (Cody::Server *s, Cody::Flags,
				      std::string &module)
{
  return cmi_response (s, module);
}

int
module_resolver::IncludeTranslateRequest (Cody::Server *s, Cody::Flags,
					  std::string &include)
{
  auto iter = map.find (include);
  if (iter == map.end () && default_translate)
    {
      // Not found, look for it
      auto file = GetCMIName (include);
      struct stat statbuf;
      bool ok = true;

#if HAVE_FSTATAT
      int fd_dir = AT_FDCWD;
      if (!repo.empty ())
	{
	  if (fd_repo == -1)
	    {
	      fd_repo = open (repo.c_str (),
			      O_RDONLY | O_CLOEXEC | O_DIRECTORY);
	      if (fd_repo < 0)
		fd_repo = -2;
	    }
	  fd_dir = fd_repo;
	}

      if (!repo.empty () && fd_repo < 0)
	ok = false;
      else if (fstatat (fd_dir, file.c_str (), &statbuf, 0) < 0
	       || !S_ISREG (statbuf.st_mode))
	ok = false;
#else
      auto append = repo;
      append.push_back (DIR_SEPARATOR);
      append.append (file);
      if (stat (append.c_str (), &statbuf) < 0
	  || !S_ISREG (statbuf.st_mode))
	ok = false;
#endif
      if (!ok)
	// Mark as not present
	file.clear ();
      auto res = map.emplace (include, file);
      iter = res.first;
    }

  if (iter == map.end () || iter->second.empty ())
    s->BoolResponse (false);
  else
    s->PathnameResponse (iter->second);

  return 0;
}

/* This handles a client notification to the server that a CMI has been
   produced for a module.  For this simplified server, we just accept
   the transaction and respond with "OK".  */

int
module_resolver::ModuleCompiledRequest (Cody::Server *s, Cody::Flags,
				      std::string &)
{
  s->OKResponse();
  return 0;
}
