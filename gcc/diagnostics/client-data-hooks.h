/* Additional metadata about a client for a diagnostic context.
   Copyright (C) 2022-2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_DIAGNOSTICS_CLIENT_DATA_HOOKS_H
#define GCC_DIAGNOSTICS_CLIENT_DATA_HOOKS_H

#include "diagnostics/logical-locations.h"

namespace diagnostics {

class sarif_object;
class client_version_info;

/* A bundle of additional metadata, owned by the diagnostics::context,
   for querying things about the client, like version data.  */

class client_data_hooks
{
 public:
  virtual ~client_data_hooks () {}

  void dump (FILE *out, int indent) const;
  void DEBUG_FUNCTION dump () const { dump (stderr, 0); }

  /* Get version info for this client, or NULL.  */
  virtual const client_version_info *get_any_version_info () const = 0;

  /* Get the current logical_locations::manager for this client, or null.  */
  virtual const logical_locations::manager *
  get_logical_location_manager () const = 0;

  /* Get the current logical location, or null.
     If this returns a non-null logical location, then
     get_logical_location_manager must return non-null.  */
  virtual logical_locations::key
  get_current_logical_location () const = 0;

  /* Get a sourceLanguage value for FILENAME, or return NULL.
     See SARIF v2.1.0 Appendix J for suggested values.  */
  virtual const char *
  maybe_get_sarif_source_language (const char *filename) const = 0;

  /* Hook to allow client to populate a SARIF "invocation" object with
     a custom property bag (see SARIF v2.1.0 section 3.8).  */
  virtual void
  add_sarif_invocation_properties (sarif_object &invocation_obj) const = 0;
};

class client_plugin_info;

/* Abstract base class for a diagnostics::context to get at
   version information about the client.  */

class client_version_info
{
public:
  class plugin_visitor
  {
  public:
    virtual void on_plugin (const client_plugin_info &) = 0;
  };

  virtual ~client_version_info () {}

  /* Get a string suitable for use as the value of the "name" property
     (SARIF v2.1.0 section 3.19.8).  */
  virtual const char *get_tool_name () const = 0;

  /* Create a string suitable for use as the value of the "fullName" property
     (SARIF v2.1.0 section 3.19.9).  */
  virtual char *maybe_make_full_name () const = 0;

  /* Get a string suitable for use as the value of the "version" property
     (SARIF v2.1.0 section 3.19.13).  */
  virtual const char *get_version_string () const = 0;

  /* Create a string suitable for use as the value of the "informationUri"
     property (SARIF v2.1.0 section 3.19.17).  */
  virtual char *maybe_make_version_url () const = 0;

  virtual void for_each_plugin (plugin_visitor &v) const = 0;
};

/* Abstract base class for a diagnostics::context to get at
   information about a specific plugin within a client.  */

class client_plugin_info
{
public:
  /* For use e.g. by SARIF "name" property (SARIF v2.1.0 section 3.19.8).  */
  virtual const char *get_short_name () const = 0;

  /* For use e.g. by SARIF "fullName" property
     (SARIF v2.1.0 section 3.19.9).  */
  virtual const char *get_full_name () const = 0;

  /* For use e.g. by SARIF "version" property
     (SARIF v2.1.0 section 3.19.13).  */
  virtual const char *get_version () const = 0;
};

} // namespace diagnostics

/* Factory function for making an instance of client_data_hooks
   for use in the compiler (i.e. with knowledge of "tree", access to
   langhooks, etc).  */

extern std::unique_ptr<diagnostics::client_data_hooks>
make_compiler_data_hooks ();

#endif /* ! GCC_DIAGNOSTICS_CLIENT_DATA_HOOKS_H */
