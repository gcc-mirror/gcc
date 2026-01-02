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

/* This file implements the domain-specific language for the options
   -fdiagnostics-add-output= and -fdiagnostics-set-output=, and for
   the "diagnostic_manager_add_sink_from_spec" entrypoint to
   libgdiagnostics.  */

#include "config.h"
#define INCLUDE_ARRAY
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "version.h"
#include "intl.h"
#include "diagnostics/color.h"
#include "diagnostics/sink.h"
#include "diagnostics/html-sink.h"
#include "diagnostics/text-sink.h"
#include "diagnostics/sarif-sink.h"
#include "selftest.h"
#include "diagnostics/selftest-context.h"
#include "pretty-print-markup.h"
#include "diagnostics/output-spec.h"

/* A namespace for handling the DSL of the arguments of
   -fdiagnostics-add-output= and -fdiagnostics-set-output=
   which look like:
     SCHEME[:KEY=VALUE(,KEY=VALUE)*]
   We call this an output spec.  */

namespace diagnostics {
namespace output_spec {

class scheme_handler;

/* Decls.  */

/* A class for the result of the first stage of parsing an output spec,
   where values are represented as untyped strings.
   The scheme might not exist.
   The keys have not been validated against the scheme.
   The values have not been validated against their keys.  */

struct scheme_name_and_params
{
  std::string m_scheme_name;
  std::vector<std::pair<std::string, std::string>> m_kvs;
};

/* Class for parsing the arguments of -fdiagnostics-add-output= and
   -fdiagnostics-set-output=, and making sink
   instances (or issuing errors).  */

class output_factory
{
public:
  output_factory (diagnostics::context &dc);

  std::unique_ptr<sink>
  make_sink (const context &ctxt,
	     diagnostics::context &dc,
	     const scheme_name_and_params &scheme_and_kvs);

  scheme_handler *get_scheme_handler (const std::string &scheme_name);

private:
  std::vector<std::unique_ptr<scheme_handler>> m_scheme_handlers;
};

enum key_handler::result
key_handler::parse_bool_value (const context &ctxt,
			       const std::string &key,
			       const std::string &value,
			       bool &out) const
{
  if (value == "yes")
    {
      out = true;
      return result::ok;
    }
  else if (value == "no")
    {
      out = false;
      return result::ok;
    }
  else
    {
      ctxt.report_error
	("%<%s%s%>:"
	 " unexpected value %qs for key %qs; expected %qs or %qs",
	 ctxt.get_option_name (), ctxt.get_unparsed_spec (),
	 value.c_str (),
	 key.c_str (),
	 "yes", "no");
      return result::malformed_value;
    }
}

template <typename EnumType, size_t NumValues>
key_handler::result
key_handler::parse_enum_value (const context &ctxt,
			       const std::string &key,
			       const std::string &value,
			       const std::array<std::pair<const char *,
							  EnumType>,
						NumValues> &value_names,
			       EnumType &out) const
{
  for (auto &iter : value_names)
    if (value == iter.first)
      {
	out = iter.second;
	return result::ok;
      }

  auto_vec<const char *> known_values;
  for (auto iter : value_names)
    known_values.safe_push (iter.first);
  pp_markup::comma_separated_quoted_strings e (known_values);
  ctxt.report_error
    ("%<%s%s%>:"
     " unexpected value %qs for key %qs; known values: %e",
     ctxt.get_option_name (), ctxt.get_unparsed_spec (),
     value.c_str (),
     key.c_str (),
     &e);
  return result::malformed_value;
}

class text_scheme_handler : public scheme_handler
{
public:
  text_scheme_handler (diagnostics::context &dc)
  : scheme_handler ("text"),
    m_show_color (pp_show_color (dc.get_reference_printer ())),
    m_show_nesting (true),
    m_show_locations_in_nesting (true),
    m_show_levels (false)
  {
  }

  std::unique_ptr<sink>
  make_sink (const context &ctxt,
	     diagnostics::context &dc) final override;

  enum result
  maybe_handle_kv (const context &ctxt,
		   const std::string &key,
		   const std::string &value) final override;

  void
  get_keys (auto_vec<const char *> &out) const final override;

private:
  bool m_show_color;
  bool m_show_nesting;
  bool m_show_locations_in_nesting;
  bool m_show_levels;
};

class sarif_scheme_handler : public scheme_handler
{
public:
  sarif_scheme_handler ()
  : scheme_handler ("sarif"),
    m_serialization_kind (sarif_serialization_kind::json)
  {
  }

  std::unique_ptr<sink>
  make_sink (const context &ctxt,
	     diagnostics::context &dc) final override;

  enum result
  maybe_handle_kv (const context &ctxt,
		   const std::string &key,
		   const std::string &value) final override;

  void
  get_keys (auto_vec<const char *> &out) const final override;

private:
  static std::unique_ptr<sarif_serialization_format>
  make_sarif_serialization_object (enum sarif_serialization_kind);

  label_text m_filename;
  enum sarif_serialization_kind m_serialization_kind;
  sarif_generation_options m_generation_opts;
};

class html_scheme_handler : public scheme_handler
{
public:
  html_scheme_handler () : scheme_handler ("experimental-html") {}

  std::unique_ptr<sink>
  make_sink (const context &ctxt,
	     diagnostics::context &dc) final override;

  enum result
  maybe_handle_kv (const context &ctxt,
		   const std::string &key,
		   const std::string &value) final override;

  void
  get_keys (auto_vec<const char *> &out) const final override;

private:
  label_text m_filename;
  html_generation_options m_html_gen_opts;
};

/* struct context.  */

void
context::report_error (const char *gmsgid, ...) const
{
  va_list ap;
  va_start (ap, gmsgid);
  report_error_va (gmsgid, &ap);
  va_end (ap);
}

void
context::report_unknown_key (const std::string &key,
			     const scheme_handler &scheme) const
{
  auto_vec<const char *> scheme_key_vec;
  scheme.get_keys (scheme_key_vec);

  pp_markup::comma_separated_quoted_strings e_scheme_keys (scheme_key_vec);

  const char *scheme_name = scheme.get_scheme_name ().c_str ();

  if (m_client_keys)
    {
      auto_vec<const char *> client_key_vec;
      m_client_keys->get_keys (client_key_vec);
      if (!client_key_vec.is_empty ())
	{
	  pp_markup::comma_separated_quoted_strings e_client_keys
	    (client_key_vec);
	  report_error
	    ("%<%s%s%>:"
	     " unknown key %qs for output scheme %qs;"
	     " scheme keys: %e; client keys: %e",
	     get_option_name (), get_unparsed_spec (),
	     key.c_str (), scheme_name,
	     &e_scheme_keys, &e_client_keys);
	}
    }

  report_error
    ("%<%s%s%>:"
     " unknown key %qs for output scheme %qs; scheme keys: %e",
     get_option_name (), get_unparsed_spec (),
     key.c_str (), scheme_name, &e_scheme_keys);
}

void
context::report_missing_key (const std::string &key,
			     const std::string &scheme_name,
			     const char *metavar) const
{
  report_error
    ("%<%s%s%>:"
     " missing required key %qs for format %qs;"
     " try %<%s%s:%s=%s%>",
     get_option_name (), get_unparsed_spec (),
     key.c_str (), scheme_name.c_str (),
     get_option_name (), scheme_name.c_str (), key.c_str (), metavar);
}

output_file
context::open_output_file (label_text &&filename) const
{
  FILE *outf = fopen (filename.get (), "w");
  if (!outf)
    {
      report_error ("unable to open %qs: %m", filename.get ());
      return output_file (nullptr, false, std::move (filename));
    }
  return output_file (outf, true, std::move (filename));
}

static std::unique_ptr<scheme_name_and_params>
parse (const context &ctxt)
{
  scheme_name_and_params result;
  const char *const unparsed_spec = ctxt.get_unparsed_spec ();
  if (const char *const colon = strchr (unparsed_spec, ':'))
    {
      result.m_scheme_name = std::string (unparsed_spec, colon - unparsed_spec);
      /* Expect zero of more of KEY=VALUE,KEY=VALUE, etc  .*/
      const char *iter = colon + 1;
      const char *last_separator = ":";
      while (iter)
	{
	  /* Look for a non-empty key string followed by '='.  */
	  const char *eq = strchr (iter, '=');
	  if (eq == nullptr || eq == iter)
	    {
	      /* Missing '='.  */
	      ctxt.report_error
		("%<%s%s%>:"
		 " expected KEY=VALUE-style parameter for format %qs"
		 " after %qs;"
		 " got %qs",
		 ctxt.get_option_name (), ctxt.get_unparsed_spec (),
		 result.m_scheme_name.c_str (),
		 last_separator,
		 iter);
	      return nullptr;
	    }
	  std::string key = std::string (iter, eq - iter);
	  std::string value;
	  const char *comma = strchr (iter, ',');
	  if (comma)
	    {
	      value = std::string (eq + 1, comma - (eq + 1));
	      iter = comma + 1;
	      last_separator = ",";
	    }
	  else
	    {
	      value = std::string (eq + 1);
	      iter = nullptr;
	    }
	  result.m_kvs.push_back ({std::move (key), std::move (value)});
	}
    }
  else
    result.m_scheme_name = unparsed_spec;
  return std::make_unique<scheme_name_and_params> (std::move (result));
}

std::unique_ptr<sink>
context::parse_and_make_sink (diagnostics::context &dc)
{
  auto parsed_arg = parse (*this);
  if (!parsed_arg)
    return nullptr;

  output_factory factory (dc);
  return factory.make_sink (*this, dc, *parsed_arg);
}

/* class scheme_handler.  */

/* class output_factory.  */

output_factory::output_factory (diagnostics::context &dc)
{
  m_scheme_handlers.push_back (std::make_unique<text_scheme_handler> (dc));
  m_scheme_handlers.push_back (std::make_unique<sarif_scheme_handler> ());
  m_scheme_handlers.push_back (std::make_unique<html_scheme_handler> ());
}

scheme_handler *
output_factory::get_scheme_handler (const std::string &scheme_name)
{
  for (auto &iter : m_scheme_handlers)
    if (iter->get_scheme_name () == scheme_name)
      return iter.get ();
  return nullptr;
}

std::unique_ptr<sink>
output_factory::make_sink (const context &ctxt,
			   diagnostics::context &dc,
			   const scheme_name_and_params &scheme_and_kvs)
{
  auto scheme_handler = get_scheme_handler (scheme_and_kvs.m_scheme_name);
  if (!scheme_handler)
    {
      auto_vec<const char *> strings;
      for (auto &iter : m_scheme_handlers)
	strings.safe_push (iter->get_scheme_name ().c_str ());
      pp_markup::comma_separated_quoted_strings e (strings);
      ctxt.report_error ("%<%s%s%>:"
			 " unrecognized format %qs; known formats: %e",
			 ctxt.get_option_name (), ctxt.get_unparsed_spec (),
			 scheme_and_kvs.m_scheme_name.c_str (), &e);
      return nullptr;
    }

  /* Parse key/value pairs.  */
  for (auto& iter : scheme_and_kvs.m_kvs)
    {
      const std::string &key = iter.first;
      const std::string &value = iter.second;
      if (!ctxt.handle_kv (key, value, *scheme_handler))
	return nullptr;
    }

  return scheme_handler->make_sink (ctxt, dc);
}

bool
context::handle_kv (const std::string &key,
		    const std::string &value,
		    scheme_handler &scheme) const
{
  auto result = scheme.maybe_handle_kv (*this, key, value);
  switch (result)
    {
    default: gcc_unreachable ();
    case key_handler::result::ok:
      return true;
    case key_handler::result::malformed_value:
      return false;
    case key_handler::result::unrecognized:
      /* Key recognized by the scheme; try the client keys.  */
      if (m_client_keys)
	{
	  result = m_client_keys->maybe_handle_kv (*this, key, value);
	  switch (result)
	    {
	    default: gcc_unreachable ();
	    case key_handler::result::ok:
	      return true;
	    case key_handler::result::malformed_value:
	      return false;
	    case key_handler::result::unrecognized:
	      break;
	    }
	}
      report_unknown_key (key, scheme);
      return false;
    }
}

/* class text_scheme_handler : public scheme_handler.  */

std::unique_ptr<sink>
text_scheme_handler::make_sink (const context &,
				diagnostics::context &dc)
{
  auto sink = std::make_unique<diagnostics::text_sink> (dc);
  sink->set_show_nesting (m_show_nesting);
  sink->set_show_locations_in_nesting (m_show_locations_in_nesting);
  sink->set_show_nesting_levels (m_show_levels);
  pp_show_color (sink->get_printer ()) = m_show_color;
  return sink;
}

enum key_handler::result
text_scheme_handler::maybe_handle_kv (const context &ctxt,
				      const std::string &key,
				      const std::string &value)
{
  if (key == "color")
    return parse_bool_value (ctxt, key, value, m_show_color);
  if (key == "show-nesting")
    return parse_bool_value (ctxt, key, value, m_show_nesting);
  if (key == "show-nesting-locations")
    return parse_bool_value (ctxt, key, value,
			     m_show_locations_in_nesting);
  if (key == "show-nesting-levels")
    return parse_bool_value (ctxt, key, value, m_show_levels);

  return result::unrecognized;
}

void
text_scheme_handler::get_keys (auto_vec<const char *> &out) const
{
  out.safe_push ("color");
  out.safe_push ("show-nesting");
  out.safe_push ("show-nesting-locations");
  out.safe_push ("show-nesting-levels");
}

/* class sarif_scheme_handler : public scheme_handler.  */

std::unique_ptr<sink>
sarif_scheme_handler::
make_sink (const context &ctxt,
	   diagnostics::context &dc)
{
  output_file output_file_;
  if (m_filename.get ())
    output_file_ = ctxt.open_output_file (std::move (m_filename));
  else
    // Default filename
    {
      const char *basename = ctxt.get_base_filename ();
      if (!basename)
	{
	  ctxt.report_missing_key ("file",
				   get_scheme_name (),
				   "FILENAME");
	  return nullptr;
	}
      output_file_
	= open_sarif_output_file (dc,
				  ctxt.get_affected_location_mgr (),
				  basename,
				  m_serialization_kind);
    }
  if (!output_file_)
    return nullptr;

  auto serialization_obj
    = make_sarif_serialization_object (m_serialization_kind);

  auto sink = make_sarif_sink (dc,
			       *ctxt.get_affected_location_mgr (),
			       std::move (serialization_obj),
			       m_generation_opts,
			       std::move (output_file_));

  return sink;
}

enum key_handler::result
sarif_scheme_handler::maybe_handle_kv (const context &ctxt,
				       const std::string &key,
				       const std::string &value)
{
  if (key == "file")
    {
      m_filename = label_text::take (xstrdup (value.c_str ()));
      return result::ok;
    }
  if (key == "serialization")
    {
      static const std::array<std::pair<const char *, enum sarif_serialization_kind>,
			      (size_t)sarif_serialization_kind::num_values> value_names
	{{{"json", sarif_serialization_kind::json}}};
      return parse_enum_value<enum sarif_serialization_kind>
	(ctxt,
	 key, value,
	 value_names,
	 m_serialization_kind);
    }
  if (key == "version")
    {
      static const std::array<std::pair<const char *, enum sarif_version>,
			      (size_t)sarif_version::num_versions> value_names
	{{{"2.1", sarif_version::v2_1_0},
	  {"2.2-prerelease", sarif_version::v2_2_prerelease_2024_08_08}}};
      return parse_enum_value<enum sarif_version>
	(ctxt,
	 key, value,
	 value_names,
	 m_generation_opts.m_version);
    }
  if (key == "state-graphs")
    return parse_bool_value (ctxt, key, value,
			     m_generation_opts.m_state_graph);

  return result::unrecognized;
}

void
sarif_scheme_handler::get_keys (auto_vec<const char *> &out) const
{
  out.safe_push ("file");
  out.safe_push ("serialization");
  out.safe_push ("state-graphs");
  out.safe_push ("version");
}

std::unique_ptr<sarif_serialization_format>
sarif_scheme_handler::
make_sarif_serialization_object (enum sarif_serialization_kind kind)
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case sarif_serialization_kind::json:
      return std::make_unique<sarif_serialization_format_json> (true);
      break;
    }
}

/* class html_scheme_handler : public scheme_handler.  */

std::unique_ptr<sink>
html_scheme_handler::
make_sink (const context &ctxt,
	   diagnostics::context &dc)
{
  output_file output_file_;
  if (m_filename.get ())
    output_file_ = ctxt.open_output_file (std::move (m_filename));
  else
    // Default filename
    {
      const char *basename = ctxt.get_base_filename ();
      if (!basename)
	{
	  ctxt.report_missing_key ("file",
				   get_scheme_name (),
				   "FILENAME");
	  return nullptr;
	}
      output_file_
	= open_html_output_file
	    (dc,
	     ctxt.get_affected_location_mgr (),
	     basename);
    }
  if (!output_file_)
    return nullptr;

  auto sink = make_html_sink (dc,
			      *ctxt.get_affected_location_mgr (),
			      m_html_gen_opts,
			      std::move (output_file_));
  return sink;
}

enum key_handler::result
html_scheme_handler::maybe_handle_kv (const context &ctxt,
				      const std::string &key,
				      const std::string &value)
{
  if (key == "css")
    return parse_bool_value (ctxt, key, value, m_html_gen_opts.m_css);
  if (key == "file")
    {
      m_filename = label_text::take (xstrdup (value.c_str ()));
      return result::ok;
    }
  if (key == "javascript")
    return parse_bool_value (ctxt, key, value,
			     m_html_gen_opts.m_javascript);
  if (key == "show-state-diagrams")
    return parse_bool_value (ctxt, key, value,
			     m_html_gen_opts.m_show_state_diagrams);
  if (key == "show-graph-dot-src")
    return parse_bool_value (ctxt, key, value,
			     m_html_gen_opts.m_show_graph_dot_src);
  if (key == "show-graph-sarif")
    return parse_bool_value (ctxt, key, value,
			     m_html_gen_opts.m_show_graph_sarif);
  return result::unrecognized;
}

void
html_scheme_handler::get_keys (auto_vec<const char *> &out) const
{
  out.safe_push ("css");
  out.safe_push ("file");
  out.safe_push ("javascript");
  out.safe_push ("show-state-diagrams");
  out.safe_push ("show-graph-dot-src");
  out.safe_push ("show-graph-sarif");
}

} // namespace output_spec

#if CHECKING_P

namespace selftest {

using auto_fix_quotes = ::selftest::auto_fix_quotes;

/* RAII class to temporarily override "progname" to the
   string "PROGNAME".  */

class auto_fix_progname
{
public:
  auto_fix_progname ()
  {
    m_old_progname = progname;
    progname = "PROGNAME";
  }

  ~auto_fix_progname ()
  {
    progname = m_old_progname;
  }

private:
  const char *m_old_progname;
};

struct parser_test
{
  class test_spec_context : public diagnostics::output_spec::dc_spec_context
  {
  public:
    test_spec_context (const char *option_name,
		       const char *unparsed_spec,
		       diagnostics::output_spec::key_handler *client_keys,
		       line_maps *location_mgr,
		       diagnostics::context &dc,
		       location_t loc)
    : dc_spec_context (option_name,
		       unparsed_spec,
		       client_keys,
		       location_mgr,
		       dc,
		       location_mgr,
		       loc)
    {
    }

    const char *
    get_base_filename () const final override
    {
      return "BASE_FILENAME";
    }
  };

  parser_test (const char *unparsed_spec,
	       diagnostics::output_spec::key_handler *client_keys = nullptr)
  : m_dc (),
    m_ctxt ("-fOPTION=",
	    unparsed_spec,
	    client_keys,
	    line_table,
	    m_dc,
	    UNKNOWN_LOCATION),
    m_fmt (m_dc.get_sink (0))
  {
    pp_buffer (m_fmt.get_printer ())->m_flush_p = false;
  }

  std::unique_ptr<diagnostics::output_spec::scheme_name_and_params>
  parse ()
  {
    return diagnostics::output_spec::parse (m_ctxt);
  }

  std::unique_ptr<diagnostics::sink>
  parse_and_make_sink ()
  {
    return m_ctxt.parse_and_make_sink (m_dc);
  }

  bool execution_failed_p () const
  {
    return m_dc.execution_failed_p ();
  }

  const char *
  get_diagnostic_text () const
  {
    return pp_formatted_text (m_fmt.get_printer ());
  }

private:
  diagnostics::selftest::test_context m_dc;
  test_spec_context m_ctxt;
  diagnostics::sink &m_fmt;
};

/* Selftests.  */

static void
test_output_arg_parsing ()
{
  /* Minimal correct example.  */
  {
    parser_test pt ("foo");
    auto result = pt.parse ();
    ASSERT_EQ (result->m_scheme_name, "foo");
    ASSERT_EQ (result->m_kvs.size (), 0);
    ASSERT_FALSE (pt.execution_failed_p ());
  }

  /* Stray trailing colon with no key/value pairs.  */
  {
    parser_test pt ("foo:");
    auto result = pt.parse ();
    ASSERT_EQ (result, nullptr);
    ASSERT_TRUE (pt.execution_failed_p ());
    ASSERT_STREQ (pt.get_diagnostic_text (),
		  "PROGNAME: error: `-fOPTION=foo:':"
		  " expected KEY=VALUE-style parameter for format `foo'"
		  " after `:';"
		  " got `'\n");
  }

  /* No key before '='.  */
  {
    parser_test pt ("foo:=");
    auto result = pt.parse ();
    ASSERT_EQ (result, nullptr);
    ASSERT_TRUE (pt.execution_failed_p ());
    ASSERT_STREQ (pt.get_diagnostic_text (),
		  "PROGNAME: error: `-fOPTION=foo:=':"
		  " expected KEY=VALUE-style parameter for format `foo'"
		  " after `:';"
		  " got `='\n");
  }

  /* No value for key.  */
  {
    parser_test pt ("foo:key,");
    auto result = pt.parse ();
    ASSERT_EQ (result, nullptr);
    ASSERT_TRUE (pt.execution_failed_p ());
    ASSERT_STREQ (pt.get_diagnostic_text (),
		  "PROGNAME: error: `-fOPTION=foo:key,':"
		  " expected KEY=VALUE-style parameter for format `foo'"
		  " after `:';"
		  " got `key,'\n");
  }

  /* Correct example, with one key/value pair.  */
  {
    parser_test pt ("foo:key=value");
    auto result = pt.parse ();
    ASSERT_EQ (result->m_scheme_name, "foo");
    ASSERT_EQ (result->m_kvs.size (), 1);
    ASSERT_EQ (result->m_kvs[0].first, "key");
    ASSERT_EQ (result->m_kvs[0].second, "value");
    ASSERT_FALSE (pt.execution_failed_p ());
  }

  /* Stray trailing comma.  */
  {
    parser_test pt ("foo:key=value,");
    auto result = pt.parse ();
    ASSERT_EQ (result, nullptr);
    ASSERT_TRUE (pt.execution_failed_p ());
    ASSERT_STREQ (pt.get_diagnostic_text (),
		  "PROGNAME: error: `-fOPTION=foo:key=value,':"
		  " expected KEY=VALUE-style parameter for format `foo'"
		  " after `,';"
		  " got `'\n");
  }

  /* Correct example, with two key/value pairs.  */
  {
    parser_test pt ("foo:color=red,shape=circle");
    auto result = pt.parse ();
    ASSERT_EQ (result->m_scheme_name, "foo");
    ASSERT_EQ (result->m_kvs.size (), 2);
    ASSERT_EQ (result->m_kvs[0].first, "color");
    ASSERT_EQ (result->m_kvs[0].second, "red");
    ASSERT_EQ (result->m_kvs[1].first, "shape");
    ASSERT_EQ (result->m_kvs[1].second, "circle");
    ASSERT_FALSE (pt.execution_failed_p ());
  }
}

class test_key_handler : public diagnostics::output_spec::key_handler
{
public:
  test_key_handler ()
  : m_verbose (false),
    m_strict (false)
  {
  }

  enum result
  maybe_handle_kv (const diagnostics::output_spec::context &ctxt,
		   const std::string &key,
		   const std::string &value) final override
  {
    if (key == "verbose")
      return parse_bool_value (ctxt, key, value, m_verbose);
    if (key == "strict")
      return parse_bool_value (ctxt, key, value, m_strict);
    return result::unrecognized;
  }

  void
  get_keys (auto_vec<const char *> &out_known_keys) const final override
  {
    out_known_keys.safe_push ("verbose");
    out_known_keys.safe_push ("strict");
  }

  bool m_verbose;
  bool m_strict;
};

static void
test_client_arg_parsing ()
{
  test_key_handler client_keys;
  parser_test pt ("text:verbose=yes,strict=no", &client_keys);
  auto result = pt.parse_and_make_sink ();
  ASSERT_TRUE (result.get ());
  ASSERT_TRUE (client_keys.m_verbose);
  ASSERT_FALSE (client_keys.m_strict);
}

/* Run all of the selftests within this file.  */

void
output_spec_cc_tests ()
{
  auto_fix_quotes fix_quotes;
  auto_fix_progname fix_progname;

  test_output_arg_parsing ();
  test_client_arg_parsing ();
}

} // namespace diagnostics::selftest

#endif /* #if CHECKING_P */

} // namespace diagnostics
