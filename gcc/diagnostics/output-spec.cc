/* Support for the DSL of -fdiagnostics-add-output= and
   -fdiagnostics-set-output=.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.

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
#include "diagnostic.h"
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
   -fdiagnostics-add-output= and -fdiagnostics-set-output=.  */

namespace diagnostics {
namespace output_spec {

/* Decls.  */

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
  class scheme_handler
  {
  public:
    scheme_handler (std::string scheme_name)
    : m_scheme_name (std::move (scheme_name))
    {}
    virtual ~scheme_handler () {}

    const std::string &get_scheme_name () const { return m_scheme_name; }

    virtual std::unique_ptr<sink>
    make_sink (const context &ctxt,
	       diagnostics::context &dc,
	       const char *unparsed_arg,
	       const scheme_name_and_params &parsed_arg) const = 0;

  protected:
    bool
    parse_bool_value (const context &ctxt,
		      const char *unparsed_arg,
		      const std::string &key,
		      const std::string &value,
		      bool &out) const
    {
      if (value == "yes")
	{
	  out = true;
	  return true;
	}
      else if (value == "no")
	{
	  out = false;
	  return true;
	}
      else
	{
	  ctxt.report_error
	    ("%<%s%s%>:"
	     " unexpected value %qs for key %qs; expected %qs or %qs",
	     ctxt.get_option_name (), unparsed_arg,
	     value.c_str (),
	     key.c_str (),
	     "yes", "no");

	  return false;
	}
    }
    template <typename EnumType, size_t NumValues>
    bool
    parse_enum_value (const context &ctxt,
		      const char *unparsed_arg,
		      const std::string &key,
		      const std::string &value,
		      const std::array<std::pair<const char *, EnumType>, NumValues> &value_names,
		      EnumType &out) const
    {
      for (auto &iter : value_names)
	if (value == iter.first)
	  {
	    out = iter.second;
	    return true;
	  }

      auto_vec<const char *> known_values;
      for (auto iter : value_names)
	known_values.safe_push (iter.first);
      pp_markup::comma_separated_quoted_strings e (known_values);
      ctxt.report_error
	("%<%s%s%>:"
	 " unexpected value %qs for key %qs; known values: %e",
	 ctxt.get_option_name (), unparsed_arg,
	 value.c_str (),
	 key.c_str (),
	 &e);
      return false;
    }

  private:
    const std::string m_scheme_name;
  };

  output_factory ();

  std::unique_ptr<sink>
  make_sink (const context &ctxt,
	     diagnostics::context &dc,
	     const char *unparsed_arg,
	     const scheme_name_and_params &parsed_arg);

  const scheme_handler *get_scheme_handler (const std::string &scheme_name);

private:
  std::vector<std::unique_ptr<scheme_handler>> m_scheme_handlers;
};

class text_scheme_handler : public output_factory::scheme_handler
{
public:
  text_scheme_handler () : scheme_handler ("text") {}

  std::unique_ptr<sink>
  make_sink (const context &ctxt,
	     diagnostics::context &dc,
	     const char *unparsed_arg,
	     const scheme_name_and_params &parsed_arg) const final override;
};

class sarif_scheme_handler : public output_factory::scheme_handler
{
public:
  sarif_scheme_handler () : scheme_handler ("sarif") {}

  std::unique_ptr<sink>
  make_sink (const context &ctxt,
	     diagnostics::context &dc,
	     const char *unparsed_arg,
	     const scheme_name_and_params &parsed_arg) const final override;

private:
  static std::unique_ptr<sarif_serialization_format>
  make_sarif_serialization_object (enum sarif_serialization_kind);
};

class html_scheme_handler : public output_factory::scheme_handler
{
public:
  html_scheme_handler () : scheme_handler ("experimental-html") {}

  std::unique_ptr<sink>
  make_sink (const context &ctxt,
	     diagnostics::context &dc,
	     const char *unparsed_arg,
	     const scheme_name_and_params &parsed_arg) const final override;
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
context::report_unknown_key (const char *unparsed_arg,
			     const std::string &key,
			     const std::string &scheme_name,
			     auto_vec<const char *> &known_keys) const
{
  pp_markup::comma_separated_quoted_strings e (known_keys);
  report_error
    ("%<%s%s%>:"
     " unknown key %qs for format %qs; known keys: %e",
     get_option_name (), unparsed_arg,
     key.c_str (), scheme_name.c_str (), &e);
}

void
context::report_missing_key (const char *unparsed_arg,
			     const std::string &key,
			     const std::string &scheme_name,
			     const char *metavar) const
{
  report_error
    ("%<%s%s%>:"
     " missing required key %qs for format %qs;"
     " try %<%s%s:%s=%s%>",
     get_option_name (), unparsed_arg,
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
parse (const context &ctxt, const char *unparsed_arg)
{
  scheme_name_and_params result;
  if (const char *const colon = strchr (unparsed_arg, ':'))
    {
      result.m_scheme_name = std::string (unparsed_arg, colon - unparsed_arg);
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
		 ctxt.get_option_name (), unparsed_arg,
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
    result.m_scheme_name = unparsed_arg;
  return std::make_unique<scheme_name_and_params> (std::move (result));
}

std::unique_ptr<sink>
context::parse_and_make_sink (const char *unparsed_arg,
			      diagnostics::context &dc)
{
  auto parsed_arg = parse (*this, unparsed_arg);
  if (!parsed_arg)
    return nullptr;

  output_factory factory;
  return factory.make_sink (*this, dc, unparsed_arg, *parsed_arg);
}

/* class output_factory::scheme_handler.  */

/* class output_factory.  */

output_factory::output_factory ()
{
  m_scheme_handlers.push_back (std::make_unique<text_scheme_handler> ());
  m_scheme_handlers.push_back (std::make_unique<sarif_scheme_handler> ());
  m_scheme_handlers.push_back (std::make_unique<html_scheme_handler> ());
}

const output_factory::scheme_handler *
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
			   const char *unparsed_arg,
			   const scheme_name_and_params &parsed_arg)
{
  auto scheme_handler = get_scheme_handler (parsed_arg.m_scheme_name);
  if (!scheme_handler)
    {
      auto_vec<const char *> strings;
      for (auto &iter : m_scheme_handlers)
	strings.safe_push (iter->get_scheme_name ().c_str ());
      pp_markup::comma_separated_quoted_strings e (strings);
      ctxt.report_error ("%<%s%s%>:"
			 " unrecognized format %qs; known formats: %e",
			 ctxt.get_option_name (), unparsed_arg,
			 parsed_arg.m_scheme_name.c_str (), &e);
      return nullptr;
    }

  return scheme_handler->make_sink (ctxt, dc, unparsed_arg, parsed_arg);
}

/* class text_scheme_handler : public output_factory::scheme_handler.  */

std::unique_ptr<sink>
text_scheme_handler::make_sink (const context &ctxt,
				diagnostics::context &dc,
				const char *unparsed_arg,
				const scheme_name_and_params &parsed_arg) const
{
  bool show_color = pp_show_color (dc.get_reference_printer ());
  bool show_nesting = true;
  bool show_locations_in_nesting = true;
  bool show_levels = false;
  for (auto& iter : parsed_arg.m_kvs)
    {
      const std::string &key = iter.first;
      const std::string &value = iter.second;
      if (key == "color")
	{
	  if (!parse_bool_value (ctxt, unparsed_arg, key, value, show_color))
	    return nullptr;
	  continue;
	}
      if (key == "show-nesting")
	{
	  if (!parse_bool_value (ctxt, unparsed_arg, key, value,
				 show_nesting))
	    return nullptr;
	  continue;
	}
      if (key == "show-nesting-locations")
	{
	  if (!parse_bool_value (ctxt, unparsed_arg, key, value,
				 show_locations_in_nesting))
	    return nullptr;
	  continue;
	}
      if (key == "show-nesting-levels")
	{
	  if (!parse_bool_value (ctxt, unparsed_arg, key, value, show_levels))
	    return nullptr;
	  continue;
	}

      /* Key not found.  */
      auto_vec<const char *> known_keys;
      known_keys.safe_push ("color");
      known_keys.safe_push ("show-nesting");
      known_keys.safe_push ("show-nesting-locations");
      known_keys.safe_push ("show-nesting-levels");
      ctxt.report_unknown_key (unparsed_arg, key, get_scheme_name (),
			       known_keys);
      return nullptr;
    }

  auto sink = std::make_unique<diagnostics::text_sink> (dc);
  sink->set_show_nesting (show_nesting);
  sink->set_show_locations_in_nesting (show_locations_in_nesting);
  sink->set_show_nesting_levels (show_levels);
  return sink;
}

/* class sarif_scheme_handler : public output_factory::scheme_handler.  */

std::unique_ptr<sink>
sarif_scheme_handler::make_sink (const context &ctxt,
				 diagnostics::context &dc,
				 const char *unparsed_arg,
				 const scheme_name_and_params &parsed_arg) const
{
  label_text filename;
  enum sarif_serialization_kind serialization_kind
    = sarif_serialization_kind::json;
  sarif_generation_options sarif_gen_opts;
  for (auto& iter : parsed_arg.m_kvs)
    {
      const std::string &key = iter.first;
      const std::string &value = iter.second;
      if (key == "file")
	{
	  filename = label_text::take (xstrdup (value.c_str ()));
	  continue;
	}
      if (key == "serialization")
	{
	  static const std::array<std::pair<const char *, enum sarif_serialization_kind>,
				  (size_t)sarif_serialization_kind::num_values> value_names
	    {{{"json", sarif_serialization_kind::json}}};

	  if (!parse_enum_value<enum sarif_serialization_kind>
		 (ctxt, unparsed_arg,
		  key, value,
		  value_names,
		  serialization_kind))
	    return nullptr;
	  continue;
	}
      if (key == "version")
	{
	  static const std::array<std::pair<const char *, enum sarif_version>,
				  (size_t)sarif_version::num_versions> value_names
	    {{{"2.1", sarif_version::v2_1_0},
	      {"2.2-prerelease", sarif_version::v2_2_prerelease_2024_08_08}}};

	    if (!parse_enum_value<enum sarif_version>
		   (ctxt, unparsed_arg,
		    key, value,
		    value_names,
		    sarif_gen_opts.m_version))
	    return nullptr;
	  continue;
	}
      if (key == "state-graphs")
	{
	  if (!parse_bool_value (ctxt, unparsed_arg, key, value,
				 sarif_gen_opts.m_state_graph))
	    return nullptr;
	  continue;
	}

      /* Key not found.  */
      auto_vec<const char *> known_keys;
      known_keys.safe_push ("file");
      known_keys.safe_push ("serialization");
      known_keys.safe_push ("state-graphs");
      known_keys.safe_push ("version");
      ctxt.report_unknown_key (unparsed_arg, key, get_scheme_name (),
			       known_keys);
      return nullptr;
    }

  output_file output_file_;
  if (filename.get ())
    output_file_ = ctxt.open_output_file (std::move (filename));
  else
    // Default filename
    {
      const char *basename = ctxt.get_base_filename ();
      if (!basename)
	{
	  ctxt.report_missing_key (unparsed_arg,
				   "file",
				   get_scheme_name (),
				   "FILENAME");
	  return nullptr;
	}
      output_file_
	= open_sarif_output_file (dc,
				  ctxt.get_affected_location_mgr (),
				  basename,
				  serialization_kind);
    }
  if (!output_file_)
    return nullptr;

  auto serialization_obj = make_sarif_serialization_object (serialization_kind);

  auto sink = make_sarif_sink (dc,
			       *ctxt.get_affected_location_mgr (),
			       std::move (serialization_obj),
			       sarif_gen_opts,
			       std::move (output_file_));
  return sink;
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

/* class html_scheme_handler : public output_factory::scheme_handler.  */

std::unique_ptr<sink>
html_scheme_handler::make_sink (const context &ctxt,
				diagnostics::context &dc,
				const char *unparsed_arg,
				const scheme_name_and_params &parsed_arg) const
{
  label_text filename;
  html_generation_options html_gen_opts;
  for (auto& iter : parsed_arg.m_kvs)
    {
      const std::string &key = iter.first;
      const std::string &value = iter.second;
      if (key == "css")
	{
	  if (!parse_bool_value (ctxt, unparsed_arg, key, value,
				 html_gen_opts.m_css))
	    return nullptr;
	  continue;
	}
      if (key == "file")
	{
	  filename = label_text::take (xstrdup (value.c_str ()));
	  continue;
	}
      if (key == "javascript")
	{
	  if (!parse_bool_value (ctxt, unparsed_arg, key, value,
				 html_gen_opts.m_javascript))
	    return nullptr;
	  continue;
	}
      if (key == "show-state-diagrams")
	{
	  if (!parse_bool_value (ctxt, unparsed_arg, key, value,
				 html_gen_opts.m_show_state_diagrams))
	    return nullptr;
	  continue;
	}
      if (key == "show-state-diagrams-dot-src")
	{
	  if (!parse_bool_value (ctxt, unparsed_arg, key, value,
				 html_gen_opts.m_show_state_diagrams_dot_src))
	    return nullptr;
	  continue;
	}
      if (key == "show-state-diagrams-sarif")
	{
	  if (!parse_bool_value (ctxt, unparsed_arg, key, value,
				 html_gen_opts.m_show_state_diagrams_sarif))
	    return nullptr;
	  continue;
	}

      /* Key not found.  */
      auto_vec<const char *> known_keys;
      known_keys.safe_push ("css");
      known_keys.safe_push ("file");
      known_keys.safe_push ("javascript");
      known_keys.safe_push ("show-state-diagrams");
      known_keys.safe_push ("show-state-diagram-dot-src");
      known_keys.safe_push ("show-state-diagram-sarif");
      ctxt.report_unknown_key (unparsed_arg, key, get_scheme_name (),
			       known_keys);
      return nullptr;
    }

  output_file output_file_;
  if (filename.get ())
    output_file_ = ctxt.open_output_file (std::move (filename));
  else
    // Default filename
    {
      const char *basename = ctxt.get_base_filename ();
      if (!basename)
	{
	  ctxt.report_missing_key (unparsed_arg,
				   "file",
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
			      html_gen_opts,
			      std::move (output_file_));
  return sink;
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
    test_spec_context (diagnostics::context &dc,
		       line_maps *location_mgr,
		       location_t loc,
		       const char *option_name)
    : dc_spec_context (dc,
		       location_mgr,
		       location_mgr,
		       loc,
		       option_name)
    {
    }

    const char *
    get_base_filename () const final override
    {
      return "BASE_FILENAME";
    }
  };

  parser_test ()
  : m_dc (),
    m_ctxt (m_dc, line_table, UNKNOWN_LOCATION, "-fOPTION="),
    m_fmt (m_dc.get_sink (0))
  {
    pp_buffer (m_fmt.get_printer ())->m_flush_p = false;
  }

  std::unique_ptr<diagnostics::output_spec::scheme_name_and_params>
  parse (const char *unparsed_arg)
  {
    return diagnostics::output_spec::parse (m_ctxt, unparsed_arg);
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
  auto_fix_quotes fix_quotes;
  auto_fix_progname fix_progname;

  /* Minimal correct example.  */
  {
    parser_test pt;
    auto result = pt.parse ("foo");
    ASSERT_EQ (result->m_scheme_name, "foo");
    ASSERT_EQ (result->m_kvs.size (), 0);
    ASSERT_FALSE (pt.execution_failed_p ());
  }

  /* Stray trailing colon with no key/value pairs.  */
  {
    parser_test pt;
    auto result = pt.parse ("foo:");
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
    parser_test pt;
    auto result = pt.parse ("foo:=");
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
    parser_test pt;
    auto result = pt.parse ("foo:key,");
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
    parser_test pt;
    auto result = pt.parse ("foo:key=value");
    ASSERT_EQ (result->m_scheme_name, "foo");
    ASSERT_EQ (result->m_kvs.size (), 1);
    ASSERT_EQ (result->m_kvs[0].first, "key");
    ASSERT_EQ (result->m_kvs[0].second, "value");
    ASSERT_FALSE (pt.execution_failed_p ());
  }

  /* Stray trailing comma.  */
  {
    parser_test pt;
    auto result = pt.parse ("foo:key=value,");
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
    parser_test pt;
    auto result = pt.parse ("foo:color=red,shape=circle");
    ASSERT_EQ (result->m_scheme_name, "foo");
    ASSERT_EQ (result->m_kvs.size (), 2);
    ASSERT_EQ (result->m_kvs[0].first, "color");
    ASSERT_EQ (result->m_kvs[0].second, "red");
    ASSERT_EQ (result->m_kvs[1].first, "shape");
    ASSERT_EQ (result->m_kvs[1].second, "circle");
    ASSERT_FALSE (pt.execution_failed_p ());
  }
}

/* Run all of the selftests within this file.  */

void
output_spec_cc_tests ()
{
  test_output_arg_parsing ();
}

} // namespace diagnostics::selftest

#endif /* #if CHECKING_P */

} // namespace diagnostics
