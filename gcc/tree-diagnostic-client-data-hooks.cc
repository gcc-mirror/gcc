/* Implementation of diagnostic_client_data_hooks for the compilers
   (e.g. with knowledge of "tree", lang_hooks, and timevars).
   Copyright (C) 2022-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "version.h"
#include "tree.h"
#include "diagnostic.h"
#include "tree-logical-location.h"
#include "diagnostic-client-data-hooks.h"
#include "diagnostic-format-sarif.h"
#include "langhooks.h"
#include "plugin.h"
#include "timevar.h"
#include "make-unique.h"

/* Concrete class for supplying a diagnostic_context with information
   about a specific plugin within the client, when the client is the
   compiler (i.e. a GCC plugin).  */

class compiler_diagnostic_client_plugin_info
  : public diagnostic_client_plugin_info
{
public:
  compiler_diagnostic_client_plugin_info (const plugin_name_args *args)
  : m_args (args)
  {
  }

  const char *get_short_name () const final override
  {
    return m_args->base_name;
  }

  const char *get_full_name () const final override
  {
    return m_args->full_name;
  }

  const char *get_version () const final override
  {
    return m_args->version;
  }

private:
  const plugin_name_args *m_args;
};

/* Concrete subclass of client_version_info for use by compilers proper,
   (i.e. using lang_hooks, and with knowledge of GCC plugins).  */

class compiler_version_info : public client_version_info
{
public:
  const char *get_tool_name () const final override
  {
    return lang_hooks.name;
  }

  /* Compare with toplev.cc: print_version.
     TARGET_NAME is passed in by the Makefile.  */
  char *
  maybe_make_full_name () const final override
  {
    return xasprintf ("%s %sversion %s (%s)",
		      get_tool_name (), pkgversion_string, version_string,
		      TARGET_NAME);
  }

  const char *get_version_string () const final override
  {
    return version_string;
  }

  char *maybe_make_version_url () const final override
  {
    return xasprintf ("https://gcc.gnu.org/gcc-%i/", GCC_major_version);
  }

  void for_each_plugin (plugin_visitor &visitor) const final override
  {
    ::for_each_plugin (on_plugin_cb, &visitor);
  }

private:
  static void
  on_plugin_cb (const plugin_name_args *args,
		void *user_data)
  {
    compiler_diagnostic_client_plugin_info cpi (args);
    client_version_info::plugin_visitor *visitor
      = (client_version_info::plugin_visitor *)user_data;
    visitor->on_plugin (cpi);
  }
};

/* Subclass of diagnostic_client_data_hooks for use by compilers proper
   i.e. with knowledge of "tree", access to langhooks, timevars etc.  */

class compiler_data_hooks : public diagnostic_client_data_hooks
{
public:
  const client_version_info *get_any_version_info () const final override
  {
    return &m_version_info;
  }

  const logical_location *get_current_logical_location () const final override
  {
    if (current_function_decl)
      return &m_current_fndecl_logical_loc;
    else
      return NULL;
  }

  const char *
  maybe_get_sarif_source_language (const char *filename) const final override
  {
    return lang_hooks.get_sarif_source_language (filename);
  }

  void
  add_sarif_invocation_properties (sarif_object &invocation_obj)
    const final override
  {
    if (g_timer)
      if (auto timereport_val = g_timer->make_json ())
	{
	  sarif_property_bag &bag_obj
	    = invocation_obj.get_or_create_properties ();
	  bag_obj.set ("gcc/timeReport", std::move (timereport_val));

	  /* If the user requested SARIF output, then assume they want the
	     time report data in the SARIF output, and *not* later emitted on
	     stderr.
	     Implement this by cleaning up the global timer instance now.  */
	  delete g_timer;
	  g_timer = NULL;
	}
  }

private:
  compiler_version_info m_version_info;
  current_fndecl_logical_location m_current_fndecl_logical_loc;
};

/* Create a compiler_data_hooks (so that the class can be local
   to this file).  */

std::unique_ptr<diagnostic_client_data_hooks>
make_compiler_data_hooks ()
{
  return ::make_unique<compiler_data_hooks> ();
}
