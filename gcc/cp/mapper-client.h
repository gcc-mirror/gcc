/* C++ modules.  Experimental!	-*- c++ -*-
   Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

/* Forward to the header in c++tools.  */

#ifndef MAPPER_CLIENT_H
#define MAPPER_CLIENT_H 1

#include "cody.hh"

#ifndef HAVE_SIGHANDLER_T
typedef void (*sighandler_t) (int);
#endif

class module_client : public Cody::Client
{
  pex_obj *pex = nullptr;
  sighandler_t sigpipe = SIG_IGN;
  Cody::Flags flags = Cody::Flags::None;

public:
  module_client (Cody::Server *s)
    : Client (s)
  {
  }
  module_client (pex_obj *pex, int fd_from, int fd_to);

  module_client (int fd_from, int fd_to)
    : Client (fd_from, fd_to)
  {
  }

public:
  Cody::Flags get_flags () const
  {
    return flags;
  }

public:
  static module_client *open_module_client (location_t loc, const char *option,
					    void (*set_repo) (const char *),
					    char const *);
  static void close_module_client (location_t loc, module_client *);
};

#endif
