/* C++ modules.  Experimental!	-*- c++ -*-
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

#ifndef GXX_RESOLVER_H
#define GXX_RESOLVER_H 1

// Mapper interface for client and server bits
#include "cody.hh"
// C++
#if !IN_GCC
#include <string>
#include <map>
#endif

// This is a GCC class, so GCC coding conventions on new bits.  
class module_resolver : public Cody::Resolver
{
public:
  using parent = Cody::Resolver;
  using module_map = std::map<std::string, std::string>;

private:
  std::string repo;
  std::string ident;
  module_map map;
  int fd_repo = -1;
  bool default_map = true;
  bool default_translate = true;

public:
  module_resolver (bool map = true, bool xlate = false);
  virtual ~module_resolver () override;

public:
  void set_default_map (bool d)
  {
    default_map = d;
  }
  void set_default_translate (bool d)
  {
    default_translate = d;
  }
  void set_ident (char const *i)
  {
    ident = i;
  }
  bool set_repo (std::string &&repo, bool force = false);
  bool add_mapping (std::string &&module, std::string &&file,
		    bool force = false);

  // Return +ve line number of error, or -ve errno
  int read_tuple_file (int fd, char const *prefix, bool force = false);
  int read_tuple_file (int fd, std::string const &prefix,
			    bool force = false)
  {
    return read_tuple_file (fd, prefix.empty () ? nullptr : prefix.c_str (),
			    force);
  }

public:
  // Virtual overriders, names are controlled by Cody::Resolver
  using parent::ConnectRequest;
  virtual module_resolver *ConnectRequest (Cody::Server *, unsigned version,
					   std::string &agent,
					   std::string &ident)
    override;
  using parent::ModuleRepoRequest;
  virtual int ModuleRepoRequest (Cody::Server *) override;
  using parent::ModuleExportRequest;
  virtual int ModuleExportRequest (Cody::Server *s, Cody::Flags,
				   std::string &module)
    override;
  using parent::ModuleImportRequest;
  virtual int ModuleImportRequest (Cody::Server *s, Cody::Flags,
				   std::string &module)
    override;
  using parent::IncludeTranslateRequest;
  virtual int IncludeTranslateRequest (Cody::Server *s, Cody::Flags,
				       std::string &include)
    override;

private:
  using parent::GetCMISuffix;
  virtual char const *GetCMISuffix () override;

private:
  int cmi_response (Cody::Server *s, std::string &module);
};

#endif
