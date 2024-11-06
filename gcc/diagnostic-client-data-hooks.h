/* Additional metadata about a client for a diagnostic context.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
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

#ifndef GCC_DIAGNOSTIC_CLIENT_DATA_HOOKS_H
#define GCC_DIAGNOSTIC_CLIENT_DATA_HOOKS_H

class sarif_object;
class client_version_info;

/* A bundle of additional metadata, owned by the diagnostic_context,
   for querying things about the client, like version data.  */

class diagnostic_client_data_hooks
{
 public:
  virtual ~diagnostic_client_data_hooks () {}

  /* Get version info for this client, or NULL.  */
  virtual const client_version_info *get_any_version_info () const = 0;

  /* Get the current logical_location for this client, or NULL.  */
  virtual const logical_location *get_current_logical_location () const = 0;

  /* Get a sourceLanguage value for FILENAME, or return NULL.
     See SARIF v2.1.0 Appendix J for suggested values.  */
  virtual const char *
  maybe_get_sarif_source_language (const char *filename) const = 0;

  /* Hook to allow client to populate a SARIF "invocation" object with
     a custom property bag (see SARIF v2.1.0 section 3.8).  */
  virtual void
  add_sarif_invocation_properties (sarif_object &invocation_obj) const = 0;
};

/* Factory function for making an instance of diagnostic_client_data_hooks
   for use in the compiler (i.e. with knowledge of "tree", access to
   langhooks, etc).  */

extern std::unique_ptr<diagnostic_client_data_hooks> make_compiler_data_hooks ();

class diagnostic_client_plugin_info;

/* Abstract base class for a diagnostic_context to get at
   version information about the client.  */

class client_version_info
{
public:
  class plugin_visitor
  {
  public:
    virtual void on_plugin (const diagnostic_client_plugin_info &) = 0;
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

/* Abstract base class for a diagnostic_context to get at
   information about a specific plugin within a client.  */

class diagnostic_client_plugin_info
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

#endif /* ! GCC_DIAGNOSTIC_CLIENT_DATA_HOOKS_H */
