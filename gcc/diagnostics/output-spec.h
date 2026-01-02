/* Support for the DSL of -fdiagnostics-add-output= and
   -fdiagnostics-set-output=.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

#ifndef GCC_DIAGNOSTICS_OUTPUT_SPEC_H
#define GCC_DIAGNOSTICS_OUTPUT_SPEC_H

#include "diagnostics/sink.h"
#include "diagnostics/output-file.h"

namespace diagnostics {
namespace output_spec {

class context;

/* An abstract base class for schemes, and for client-specific keys.  */

class key_handler
{
public:
  enum class result
  {
    ok,
    unrecognized,
    malformed_value
  };

  /* Attempt to decode KEY and VALUE, storing the decoded value.  */
  virtual enum result
  maybe_handle_kv (const context &ctxt,
		   const std::string &key,
		   const std::string &value) = 0;

  virtual void
  get_keys (auto_vec<const char *> &out) const = 0;

  enum result
  parse_bool_value (const context &ctxt,
		    const std::string &key,
		    const std::string &value,
		    bool &out) const;

  template <typename EnumType, size_t NumValues>
  enum result
  parse_enum_value (const context &ctxt,
		    const std::string &key,
		    const std::string &value,
		    const std::array<std::pair<const char *, EnumType>,
				     NumValues> &value_names,
		    EnumType &out) const;
};

/* Abstract subclass for handling particular schemes and their keys.  */

class scheme_handler : public key_handler
{
public:
  scheme_handler (std::string scheme_name)
    : m_scheme_name (std::move (scheme_name))
  {}
  virtual ~scheme_handler () {}

  const std::string &get_scheme_name () const { return m_scheme_name; }

  virtual std::unique_ptr<sink>
  make_sink (const context &ctxt,
	     diagnostics::context &dc) = 0;

private:
  const std::string m_scheme_name;
};

/* An abstract base class for handling the DSL of -fdiagnostics-add-output=
   and -fdiagnostics-set-output=.  */

class context
{
public:
  std::unique_ptr<sink>
  parse_and_make_sink (diagnostics::context &dc);

  void
  report_error (const char *gmsgid, ...) const
    ATTRIBUTE_GCC_DIAG(2,3);

  void
  report_unknown_key (const std::string &key,
		      const scheme_handler &scheme) const;

  void
  report_missing_key (const std::string &key,
		      const std::string &scheme_name,
		      const char *metavar) const;

  output_file
  open_output_file (label_text &&filename) const;

  const char *
  get_option_name () const { return m_option_name; }

  const char *
  get_unparsed_spec () const { return m_unparsed_spec;  }

  line_maps *
  get_affected_location_mgr () const { return m_affected_location_mgr; }

  virtual ~context () {}

  virtual void
  report_error_va (const char *gmsgid, va_list *ap) const = 0;

  virtual const char *
  get_base_filename () const = 0;

  bool
  handle_kv (const std::string &key,
	     const std::string &value,
	     scheme_handler &scheme) const;

protected:
  context (const char *option_name,
	   const char *unparsed_spec,
	   key_handler *client_keys,
	   line_maps *affected_location_mgr)
  : m_option_name (option_name),
    m_unparsed_spec (unparsed_spec),
    m_client_keys (client_keys),
    m_affected_location_mgr (affected_location_mgr)
  {
  }

  // e.g. "-fdiagnostics-add-output="
  const char *m_option_name;

  // e.g. "scheme:foo=bar,key=value"
  const char *m_unparsed_spec;

  // Optional borrowed ptr to client-specific keys
  key_handler *m_client_keys;

  line_maps *m_affected_location_mgr;
};

/* A subclass that implements reporting errors via a diagnostics::context.  */

struct dc_spec_context : public output_spec::context
{
public:
  dc_spec_context (const char *option_name,
		   const char *unparsed_spec,
		   key_handler *client_keys,
		   line_maps *affected_location_mgr,
		   diagnostics::context &dc,
		   line_maps *control_location_mgr,
		   location_t loc)
    : context (option_name,
	       unparsed_spec,
	       client_keys,
	       affected_location_mgr),
    m_dc (dc),
    m_control_location_mgr (control_location_mgr),
    m_loc (loc)
  {}

  void report_error_va (const char *gmsgid, va_list *ap) const final override
    ATTRIBUTE_GCC_DIAG(2, 0)
  {
    m_dc.begin_group ();
    rich_location richloc (m_control_location_mgr, m_loc);
    m_dc.diagnostic_impl (&richloc, nullptr, -1, gmsgid, ap, kind::error);
    m_dc.end_group ();
  }

  diagnostics::context &m_dc;
  line_maps *m_control_location_mgr;
  location_t m_loc;
};

} // namespace output_spec
} // namespace diagnostics

#endif // #ifndef GCC_DIAGNOSTICS_OUTPUT_SPEC_H
