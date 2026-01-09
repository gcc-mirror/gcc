/* HTML output for diagnostics.
   Copyright (C) 2024-2026 Free Software Foundation, Inc.
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
#define INCLUDE_MAP
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "diagnostics/metadata.h"
#include "diagnostics/sink.h"
#include "diagnostics/html-sink.h"
#include "diagnostics/text-sink.h"
#include "diagnostics/sarif-sink.h"
#include "diagnostics/output-file.h"
#include "diagnostics/buffering.h"
#include "diagnostics/paths.h"
#include "diagnostics/dumping.h"
#include "diagnostics/logging.h"
#include "diagnostics/client-data-hooks.h"
#include "selftest.h"
#include "diagnostics/selftest-context.h"
#include "pretty-print-format-impl.h"
#include "pretty-print-urlifier.h"
#include "diagnostics/changes.h"
#include "intl.h"
#include "xml.h"
#include "xml-printer.h"
#include "diagnostics/digraphs.h"
#include "diagnostics/state-graphs.h"
#include "graphviz.h"
#include "json.h"
#include "selftest-xml.h"

namespace diagnostics {

// struct html_generation_options

html_generation_options::html_generation_options ()
: m_css (true),
  m_javascript (true),
  m_show_state_diagrams (false),
  m_show_graph_sarif (false),
  m_show_graph_dot_src (false)
{
}

void
html_generation_options::dump (FILE *outfile, int indent) const
{
  DIAGNOSTICS_DUMPING_EMIT_BOOL_FIELD (m_css);
  DIAGNOSTICS_DUMPING_EMIT_BOOL_FIELD (m_javascript);
  DIAGNOSTICS_DUMPING_EMIT_BOOL_FIELD (m_show_state_diagrams);
  DIAGNOSTICS_DUMPING_EMIT_BOOL_FIELD (m_show_graph_sarif);
  DIAGNOSTICS_DUMPING_EMIT_BOOL_FIELD (m_show_graph_dot_src);
}

class html_builder;

/* Concrete buffering implementation subclass for HTML output.  */

class html_sink_buffer : public per_sink_buffer
{
public:
  friend class html_builder;
  friend class html_sink;

  html_sink_buffer (html_builder &builder)
  : m_builder (builder)
  {}

  void dump (FILE *out, int indent) const final override;
  bool empty_p () const final override;
  void move_to (per_sink_buffer &dest) final override;
  void clear () final override;
  void flush () final override;

  void add_result (std::unique_ptr<xml::element> result)
  {
    m_results.push_back (std::move (result));
  }

private:
  html_builder &m_builder;
  std::vector<std::unique_ptr<xml::element>> m_results;
};

/* A class for managing HTML output of diagnostics.

   Implemented:
   - message text

   Known limitations/missing functionality:
   - title for page
   - file/line/column
   - error vs warning
   - CWEs
   - rules
   - fix-it hints
   - paths
*/

class html_builder
{
public:
  friend class html_sink_buffer;

  html_builder (context &dc,
		pretty_printer &pp,
		const line_maps *line_maps,
		const html_generation_options &html_gen_opts);

  void dump (FILE *out, int indent) const;

  void
  set_main_input_filename (const char *name);

  void on_report_diagnostic (const diagnostic_info &diagnostic,
			     enum kind orig_diag_kind,
			     html_sink_buffer *buffer);
  void emit_diagram (const diagram &d);
  void emit_global_graph (const lazily_created<digraphs::digraph> &);
  void add_graph_for_logical_loc (const lazily_created<digraphs::digraph> &,
				  logical_locations::key);

  void end_group ();

  std::unique_ptr<xml::element> take_current_diagnostic ()
  {
    return std::move (m_cur_diagnostic_element);
  }

  void flush_to_file (FILE *outf);

  const xml::document &get_document () const { return *m_document; }

  void set_printer (pretty_printer &pp)
  {
    m_printer = &pp;
  }

  std::unique_ptr<xml::element>
  make_element_for_metadata (const metadata &);

  std::unique_ptr<xml::element>
  make_element_for_patch (const diagnostic_info &diagnostic);

  void add_focus_id (std::string focus_id)
  {
    m_ui_focus_ids.append_string (focus_id.c_str ());
  }

  std::unique_ptr<xml::node>
  maybe_make_state_diagram (const paths::event &event);

private:
  void
  add_stylesheet (std::string url);

  std::unique_ptr<xml::element>
  make_element_for_diagnostic (const diagnostic_info &diagnostic,
			       enum kind orig_diag_kind,
			       bool alert);

  std::unique_ptr<xml::element>
  make_metadata_element (label_text label,
			 label_text url);

  void
  add_at_nesting_level (size_t nesting_level,
			std::unique_ptr<xml::element> child_diag_element);

  void
  push_nesting_level ();

  void
  pop_nesting_level ();

  void
  add_graph (const digraphs::digraph &dg,
	     xml::element &parent_element);

  context &m_context;
  pretty_printer *m_printer;
  const line_maps *m_line_maps;
  html_generation_options m_html_gen_opts;
  const logical_locations::manager *m_logical_loc_mgr;

  std::unique_ptr<xml::document> m_document;
  xml::element *m_head_element;
  xml::element *m_title_element;
  xml::element *m_body_element;
  xml::element *m_diagnostics_element;
  std::unique_ptr<xml::element> m_cur_diagnostic_element;
  std::vector<xml::element *> m_cur_nesting_levels;
  int m_next_diag_id; // for handing out unique IDs
  json::array m_ui_focus_ids;
  logical_locations::key m_last_logical_location;
  location_t m_last_location;
  expanded_location m_last_expanded_location;
  std::map<logical_locations::key, xml::element *> m_per_logical_loc_graphs;
};

static std::unique_ptr<xml::element>
make_div (std::string class_)
{
  auto div = std::make_unique<xml::element> ("div", false);
  div->set_attr ("class", std::move (class_));
  return div;
}

static std::unique_ptr<xml::element>
make_span (std::string class_)
{
  auto span = std::make_unique<xml::element> ("span", true);
  span->set_attr ("class", std::move (class_));
  return span;
}

/* class html_sink_buffer : public per_sink_buffer.  */

void
html_sink_buffer::dump (FILE *out, int indent) const
{
  dumping::emit_heading (out, indent, "html_sink_buffer");
  int idx = 0;
  for (auto &result : m_results)
    {
      dumping::emit_indent (out, indent + 2);
      fprintf (out, "result[%i]:\n", idx);
      result->dump (out);
      fprintf (out, "\n");
      ++idx;
    }
}

bool
html_sink_buffer::empty_p () const
{
  return m_results.empty ();
}

void
html_sink_buffer::move_to (per_sink_buffer &base)
{
  html_sink_buffer &dest
    = static_cast<html_sink_buffer &> (base);
  for (auto &&result : m_results)
    dest.m_results.push_back (std::move (result));
  m_results.clear ();
}

void
html_sink_buffer::clear ()
{
  m_results.clear ();
}

void
html_sink_buffer::flush ()
{
  for (auto &&result : m_results)
    m_builder.m_diagnostics_element->add_child (std::move (result));
  m_results.clear ();
}

/* class html_builder.  */

/* Style information for writing out HTML paths.
   Colors taken from https://pf3.patternfly.org/v3/styles/color-palette/ */

static const char * const HTML_STYLE
  = ("  <style>\n"
     "    .linenum { color: white;\n"
     "               background-color: #0088ce;\n"
     "               white-space: pre;\n"
     "               border-right: 1px solid black; }\n"
     "    .ruler { color: red;\n"
     "              white-space: pre; }\n"
     "    .source { color: blue;\n"
     "              background-color: white;\n"
     "              white-space: pre; }\n"
     "    .annotation { color: green;\n"
     "                  background-color: white;\n"
     "                  white-space: pre; }\n"
     "    .linenum-gap { text-align: center;\n"
     "                   border-top: 1px solid black;\n"
     "                   border-right: 1px solid black;\n"
     "                   background-color: #ededed; }\n"
     "    .source-gap { border-bottom: 1px dashed black;\n"
     "                  border-top: 1px dashed black;\n"
     "                  background-color: #ededed; }\n"
     "    .no-locus-event { font-family: monospace;\n"
     "                      color: green;\n"
     "                      white-space: pre; }\n"
     "    .funcname { font-weight: bold; }\n"
     "    .events-hdr { color: white;\n"
     "                  background-color: #030303; }\n"
     "    .event-range {  border: 1px solid black;\n"
     "                    padding: 0px; }\n"
     "    .event-range-with-margin { border-spacing: 0; }\n"
     "    .locus { font-family: monospace;\n"
     "             border-spacing: 0px; }\n"
     "    .selected { color: white;\n"
     "                background-color: #0088ce; }\n"
     "    .stack-frame-with-margin { border-spacing: 0; }\n"
     "    .stack-frame {  padding: 5px;\n"
     "                    box-shadow: 0 5px 10px 0 rgba(0, 0, 0, 0.5); }\n"
     "    .frame-funcname { text-align: right;\n"
     "                      font-style: italic; } \n"
     "    .highlight-a { color: #703fec;\n" // pf-purple-400
     "                   font-weight: bold; }\n"
     "    .highlight-b { color: #3f9c35;\n" // pf-green-400
     "                   font-weight: bold; }\n"
     "    .gcc-quoted-text { font-weight: bold;\n"
     "                       font-family: mono; }\n"
     "  </style>\n");

/* A little JavaScript for ease of navigation.
   Keys j/k move forward and backward cyclically through a list
   of focus ids (written out in another <script> tag as the HTML
   is flushed).  */

const char * const HTML_SCRIPT
  = ("  var current_focus_idx = 0;\n"
     "\n"
     "  function get_focus_span (focus_idx)\n"
     "  {\n"
     "      const element_id = focus_ids[focus_idx];\n"
     "      return document.getElementById(element_id);\n"
     "  }\n"
     "  function get_any_state_diagram (focus_idx)\n"
     "  {\n"
     "      const element_id = focus_ids[focus_idx];\n"
     "      return document.getElementById(element_id + \"-state-diagram\");\n"
     "  }\n"
     "  function unhighlight_current_focus_idx ()\n"
     "  {\n"
     "      get_focus_span (current_focus_idx).classList.remove ('selected');\n"
     "      state_diagram = get_any_state_diagram (current_focus_idx);\n"
     "      if (state_diagram) {\n"
     "          state_diagram.style.visibility = \"hidden\";\n"
     "      }\n"
     "  }\n"
     "  function highlight_current_focus_idx ()\n"
     "  {\n"
     "      const el = get_focus_span (current_focus_idx);\n"
     "      el.classList.add ('selected');\n"
     "      state_diagram = get_any_state_diagram (current_focus_idx);\n"
     "      if (state_diagram) {\n"
     "          state_diagram.style.visibility = \"visible\";\n"
     "      }\n"
     "      // Center the element on the screen\n"
     "      const top_y = el.getBoundingClientRect ().top + window.pageYOffset;\n"
     "      const middle = top_y - (window.innerHeight / 2);\n"
     "      window.scrollTo (0, middle);\n"
     "  }\n"
     "  function select_prev_focus_idx ()\n"
     "  {\n"
     "      unhighlight_current_focus_idx ();\n"
     "      if (current_focus_idx > 0)\n"
     "          current_focus_idx -= 1;\n"
     "      else\n"
     "          current_focus_idx = focus_ids.length - 1;\n"
     "      highlight_current_focus_idx ();\n"
     "  }\n"
     "  function select_next_focus_idx ()\n"
     "  {\n"
     "      unhighlight_current_focus_idx ();\n"
     "      if (current_focus_idx < focus_ids.length - 1)\n"
     "          current_focus_idx += 1;\n"
     "      else\n"
     "          current_focus_idx = 0;\n"
     "      highlight_current_focus_idx ();\n"
     "  }\n"
     "  document.addEventListener('keydown', function (ev) {\n"
     "      if (ev.key == 'j')\n"
     "          select_next_focus_idx ();\n"
     "      else if (ev.key == 'k')\n"
     "          select_prev_focus_idx ();\n"
     "  });\n"
     "  highlight_current_focus_idx ();\n");

struct html_doctypedecl : public xml::doctypedecl
{
  void write_as_xml (pretty_printer *pp,
		     int depth, bool indent) const final override
  {
    if (indent)
      {
	for (int i = 0; i < depth; ++i)
	  pp_string (pp, "  ");
      }
    pp_string (pp, "<!DOCTYPE html\n"
	       "     PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
	       "     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">");
    if (indent)
      pp_newline (pp);
  }
};

/* html_builder's ctor.  */

html_builder::html_builder (context &dc,
			    pretty_printer &pp,
			    const line_maps *line_maps,
			    const html_generation_options &html_gen_opts)
: m_context (dc),
  m_printer (&pp),
  m_line_maps (line_maps),
  m_html_gen_opts (html_gen_opts),
  m_logical_loc_mgr (nullptr),
  m_head_element (nullptr),
  m_title_element (nullptr),
  m_body_element (nullptr),
  m_diagnostics_element (nullptr),
  m_next_diag_id (0),
  m_last_location (UNKNOWN_LOCATION),
  m_last_expanded_location ({})
{
  gcc_assert (m_line_maps);

  if (auto client_data_hooks = dc.get_client_data_hooks ())
    m_logical_loc_mgr = client_data_hooks->get_logical_location_manager ();

  m_document = std::make_unique<xml::document> ();
  m_document->m_doctypedecl = std::make_unique<html_doctypedecl> ();
  {
    auto html_element = std::make_unique<xml::element> ("html", false);
    html_element->set_attr ("xmlns",
			    "http://www.w3.org/1999/xhtml");
    xml::printer xp (*html_element.get ());
    m_document->add_child (std::move (html_element));

    {
      xml::auto_print_element head (xp, "head");
      m_head_element = xp.get_insertion_point ();
      {
	xml::auto_print_element title (xp, "title", true);
	m_title_element = xp.get_insertion_point ();
	m_title_element->add_text (" ");
      }

      if (m_html_gen_opts.m_css)
	{
	  add_stylesheet ("https://cdnjs.cloudflare.com/ajax/libs/patternfly/3.24.0/css/patternfly.min.css");
	  add_stylesheet ("https://cdnjs.cloudflare.com/ajax/libs/patternfly/3.24.0/css/patternfly-additions.min.css");
	  xp.add_raw (HTML_STYLE);
	}
      if (m_html_gen_opts.m_javascript)
	{
	  xp.push_tag ("script");
	  /* Escaping rules are different for HTML <script> elements,
	     so add the script "raw" for now.  */
	  xp.add_raw (HTML_SCRIPT);
	  xp.pop_tag ("script");
	}
    }

    {
      xml::auto_print_element body (xp, "body");
      m_body_element = xp.get_insertion_point ();
      {
	auto diagnostics_element = make_div ("gcc-diagnostic-list");
	m_diagnostics_element = diagnostics_element.get ();
	xp.append (std::move (diagnostics_element));
      }
    }
  }
}

void
html_builder::dump (FILE *out, int indent) const
{
  dumping::emit_heading (out, indent, "HTML generation options");
  m_html_gen_opts.dump (out, indent + 2);
}

void
html_builder::set_main_input_filename (const char *name)
{
  gcc_assert (m_title_element);
  if (name)
    {
      m_title_element->m_children.clear ();
      m_title_element->add_text (name);
    }
}

void
html_builder::add_stylesheet (std::string url)
{
  gcc_assert (m_head_element);

  xml::printer xp (*m_head_element);
  xp.push_tag ("link", false);
  xp.set_attr ("rel", "stylesheet");
  xp.set_attr ("type", "text/css");
  xp.set_attr ("href", std::move (url));
}

/* Implementation of "on_report_diagnostic" for HTML output.  */

void
html_builder::on_report_diagnostic (const diagnostic_info &diagnostic,
				    enum kind orig_diag_kind,
				    html_sink_buffer *buffer)
{
  if (diagnostic.m_kind == kind::ice || diagnostic.m_kind == kind::ice_nobt)
    {
      /* Print a header for the remaining output to stderr, and
	 return, attempting to print the usual ICE messages to
	 stderr.  Hopefully this will be helpful to the user in
	 indicating what's gone wrong (also for DejaGnu, for pruning
	 those messages).   */
      fnotice (stderr, "Internal compiler error:\n");
    }

  const int nesting_level = m_context.get_diagnostic_nesting_level ();
  bool alert = true;
  if (m_cur_diagnostic_element && nesting_level > 0)
    alert = false;
  if (!m_cur_diagnostic_element)
    m_last_logical_location = logical_locations::key ();
  auto diag_element
    = make_element_for_diagnostic (diagnostic, orig_diag_kind, alert);
  if (buffer)
    {
      gcc_assert (!m_cur_diagnostic_element);
      buffer->m_results.push_back (std::move (diag_element));
    }
  else
    {
      if (m_cur_diagnostic_element)
	{
	  /* Nested diagnostic.  */
	  gcc_assert (nesting_level >= 0);
	  add_at_nesting_level (nesting_level, std::move (diag_element));
	}
      else
	/* Top-level diagnostic.  */
	{
	  m_cur_diagnostic_element = std::move (diag_element);
	  m_cur_nesting_levels.clear ();
	}
    }
}

// For ease of comparison with show-nesting-levels=yes

static void
add_nesting_level_attr (xml::element &element,
			int nesting_level)
{
  element.set_attr ("nesting-level", std::to_string (nesting_level));
}

void
html_builder::
add_at_nesting_level (size_t nesting_level,
		      std::unique_ptr<xml::element> child_diag_element)
{
  gcc_assert (m_cur_diagnostic_element);
  while (nesting_level > m_cur_nesting_levels.size ())
    push_nesting_level ();
  while (nesting_level < m_cur_nesting_levels.size ())
    pop_nesting_level ();

  if (nesting_level > 0)
    {
      gcc_assert (!m_cur_nesting_levels.empty ());
      auto current_nesting_level = m_cur_nesting_levels.back ();
      xml::printer xp (*current_nesting_level);
      xp.push_tag ("li");
      add_nesting_level_attr (*xp.get_insertion_point (),
			      m_cur_nesting_levels.size ());
      xp.append (std::move (child_diag_element));
      xp.pop_tag ("li");
    }
  else
    m_cur_diagnostic_element->add_child (std::move (child_diag_element));
}

void
html_builder::push_nesting_level ()
{
  gcc_assert (m_cur_diagnostic_element);
  auto new_nesting_level = std::make_unique<xml::element> ("ul", false);
  add_nesting_level_attr (*new_nesting_level,
			  m_cur_nesting_levels.size () + 1);
  xml::element *current_nesting_level = nullptr;
  if (!m_cur_nesting_levels.empty ())
    current_nesting_level = m_cur_nesting_levels.back ();
  m_cur_nesting_levels.push_back (new_nesting_level.get ());
  if (current_nesting_level)
    current_nesting_level->add_child (std::move (new_nesting_level));
  else
    m_cur_diagnostic_element->add_child (std::move (new_nesting_level));
}

void
html_builder::pop_nesting_level ()
{
  gcc_assert (m_cur_diagnostic_element);
  m_cur_nesting_levels.pop_back ();
}

static void
print_pre_source (xml::printer &xp, const char *text)
{
  xp.push_tag_with_class ("pre", "source", true);
  xp.add_text (text);
  xp.pop_tag ("pre");
}

std::unique_ptr<xml::node>
html_builder::maybe_make_state_diagram (const paths::event &event)
{
  if (!m_html_gen_opts.m_show_state_diagrams)
    return nullptr;

  if (!m_logical_loc_mgr)
    return nullptr;

  /* Get state graph; if we're going to print it later, also request
     the debug version.  */
  auto state_graph
    = event.maybe_make_diagnostic_state_graph
	(m_html_gen_opts.m_show_graph_sarif);
  if (!state_graph)
    return nullptr;

  // Convert it to .dot AST
  auto dot_graph = state_graphs::make_dot_graph (*state_graph,
						 *m_logical_loc_mgr);
  gcc_assert (dot_graph);

  auto wrapper = std::make_unique<xml::element> ("div", false);
  xml::printer xp (*wrapper);

  if (m_html_gen_opts.m_show_graph_sarif)
    {
      // For debugging, show the SARIF src inline:
      pretty_printer pp;
      state_graph->make_json_sarif_graph ()->print (&pp, true);
      print_pre_source (xp, pp_formatted_text (&pp));
    }

  if (m_html_gen_opts.m_show_graph_dot_src)
    {
      // For debugging, show the dot src inline:
      pretty_printer pp;
      dot::writer w (pp);
      dot_graph->print (w);
      print_pre_source (xp, pp_formatted_text (&pp));
    }

  // Turn the .dot into SVG and splice into place
  auto svg = dot::make_svg_from_graph (*dot_graph);
  if (svg)
    xp.append (std::move (svg));

  return wrapper;
}

/* Custom subclass of html_label_writer.
   Wrap labels within a <span> element, supplying them with event IDs.
   Add the IDs to the list of focus IDs.  */

class html_path_label_writer : public html_label_writer
{
public:
  html_path_label_writer (xml::printer &xp,
			  html_builder &builder,
			  const paths::path &path,
			  const std::string &event_id_prefix)
  : m_xp (xp),
    m_html_builder (builder),
    m_path (path),
    m_event_id_prefix (event_id_prefix),
    m_next_event_idx (0),
    m_curr_event_id ()
  {
  }

  void begin_label () final override
  {
    m_curr_event_id = m_next_event_idx++;
    m_xp.push_tag_with_class ("span", "event", true);
    m_xp.set_attr ("id", get_element_id ());
    m_html_builder.add_focus_id (get_element_id ());
  }

  void end_label () final override
  {
    const paths::event &event
      = m_path.get_event (m_curr_event_id.zero_based ());
    if (auto state_doc = m_html_builder.maybe_make_state_diagram (event))
    {
      m_xp.push_tag_with_class ("div", "state-diagram", false);
      m_xp.set_attr ("id", get_element_id () + "-state-diagram");
      m_xp.set_attr ("style",
		     ("position: absolute;"
		      " z-index: 1;"
		      " visibility: hidden;"));
      m_xp.append (std::move (state_doc));
      m_xp.pop_tag ("div");
    }

    m_xp.pop_tag ("span"); // from begin_label
  }

private:
  std::string
  get_element_id () const
  {
    gcc_assert (m_curr_event_id.known_p ());
    return (m_event_id_prefix
	    + std::to_string (m_curr_event_id.zero_based ()));
  }

  xml::printer &m_xp;
  html_builder &m_html_builder;
  const paths::path &m_path;
  const std::string &m_event_id_prefix;
  int m_next_event_idx;
  paths::event_id_t m_curr_event_id;
};

/* See https://pf3.patternfly.org/v3/pattern-library/widgets/#alerts */
static const char *
get_pf_class_for_alert_div (enum kind diag_kind)
{
  switch (diag_kind)
    {
    case kind::debug:
    case kind::note:
      return "alert alert-info";

    case kind::anachronism:
    case kind::warning:
      return "alert alert-warning";

    case kind::error:
    case kind::sorry:
    case kind::ice:
    case kind::ice_nobt:
    case kind::fatal:
      return "alert alert-danger";

    default:
      gcc_unreachable ();
    }
}

static const char *
get_pf_class_for_alert_icon (enum kind diag_kind)
{
  switch (diag_kind)
    {
    case kind::debug:
    case kind::note:
      return "pficon pficon-info";

    case kind::anachronism:
    case kind::warning:
      return "pficon pficon-warning-triangle-o";

    case kind::error:
    case kind::sorry:
    case kind::ice:
    case kind::ice_nobt:
    case kind::fatal:
      return "pficon pficon-error-circle-o";

    default:
      gcc_unreachable ();
    }
}

static const char *
get_label_for_logical_location_kind (enum logical_locations::kind kind)
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case logical_locations::kind::unknown:
      return nullptr;

    /* Kinds within executable code.  */
    case logical_locations::kind::function:
      return "Function";
    case logical_locations::kind::member:
      return "Member";
    case logical_locations::kind::module_:
      return "Module";
    case logical_locations::kind::namespace_:
      return "Namespace";
    case logical_locations::kind::type:
      return "Type";
    case logical_locations::kind::return_type:
      return "Return type";
    case logical_locations::kind::parameter:
      return "Parameter";
    case logical_locations::kind::variable:
      return "Variable";

    /* Kinds within XML or HTML documents.  */
    case logical_locations::kind::element:
      return "Element";
    case logical_locations::kind::attribute:
      return "Attribute";
    case logical_locations::kind::text:
      return "Text";
    case logical_locations::kind::comment:
      return "Comment";
    case logical_locations::kind::processing_instruction:
      return "Processing Instruction";
    case logical_locations::kind::dtd:
      return "DTD";
    case logical_locations::kind::declaration:
      return "Declaration";

  /* Kinds within JSON documents.  */
    case logical_locations::kind::object:
      return "Object";
    case logical_locations::kind::array:
      return "Array";
    case logical_locations::kind::property:
      return "Property";
    case logical_locations::kind::value:
      return "Value";
    }
}

static void
add_labelled_value (xml::printer &xp,
		    std::string id,
		    std::string label,
		    std::string value,
		    bool quote_value)
{
  xp.push_tag ("div", true);
  xp.set_attr ("id", id);
  xp.push_tag ("span");
  xp.add_text (label);
  xp.add_text (" ");
  xp.pop_tag ("span");
  xp.push_tag ("span");
  if (quote_value)
    xp.set_attr ("class", "gcc-quoted-text");
  xp.add_text (std::move (value));
  xp.pop_tag ("span");
  xp.pop_tag ("div");
}

class html_token_printer : public token_printer
{
public:
  html_token_printer (xml::element &parent_element)
    /* Ideally pp_token_lists that reach a token_printer should be
       "balanced", but for now they can have mismatching pp_tokens
       e.g. a begin_color without an end_color (PR other/120610).
       Give html_token_printer its own xml::printer as a firewall to
       limit the scope of the mismatches in the HTML.  */
    : m_xp (parent_element,
	    /* Similarly we don't check that the popped tags match.  */
	    false)
  {
  }
  void print_tokens (pretty_printer */*pp*/,
		     const pp_token_list &tokens) final override
  {
    /* Implement print_tokens by adding child elements to
       m_parent_element.  */
    for (auto iter = tokens.m_first; iter; iter = iter->m_next)
      switch (iter->m_kind)
	{
	default:
	  gcc_unreachable ();

	case pp_token::kind::text:
	  {
	    pp_token_text *sub = as_a <pp_token_text *> (iter);
	    /* The value might be in the obstack, so we may need to
	       copy it.  */
	    m_xp.add_text (sub->m_value.get ());
	    }
	  break;

	case pp_token::kind::begin_color:
	  {
	    pp_token_begin_color *sub = as_a <pp_token_begin_color *> (iter);
	    gcc_assert (sub->m_value.get ());
	    m_xp.push_tag_with_class ("span", sub->m_value.get ());
	  }
	  break;

	case pp_token::kind::end_color:
	  m_xp.pop_tag ("span");
	  break;

	case pp_token::kind::begin_quote:
	  {
	    m_xp.add_text (open_quote);
	    m_xp.push_tag_with_class ("span", "gcc-quoted-text");
	  }
	  break;
	case pp_token::kind::end_quote:
	  {
	    m_xp.pop_tag ("span");
	    m_xp.add_text (close_quote);
	  }
	  break;

	case pp_token::kind::begin_url:
	  {
	    pp_token_begin_url *sub = as_a <pp_token_begin_url *> (iter);
	    m_xp.push_tag ("a", true);
	    m_xp.set_attr ("href", sub->m_value.get ());
	  }
	  break;
	case pp_token::kind::end_url:
	  m_xp.pop_tag ("a");
	  break;

	case pp_token::kind::event_id:
	  {
	    pp_token_event_id *sub = as_a <pp_token_event_id *> (iter);
	    gcc_assert (sub->m_event_id.known_p ());
	    m_xp.add_text ("(");
	    m_xp.add_text (std::to_string (sub->m_event_id.one_based ()));
	    m_xp.add_text (")");
	  }
	  break;
	}
  }

private:
  xml::printer m_xp;
};

/* Make a <div class="gcc-diagnostic"> for DIAGNOSTIC.

   If ALERT is true, make it be a PatternFly alert (see
   https://pf3.patternfly.org/v3/pattern-library/widgets/#alerts) and
   show severity text (e.g. "error: ").

   If ALERT is false, don't show the severity text and don't show
   the filename if it's the same as the previous diagnostic within the
   diagnostic group.  */

std::unique_ptr<xml::element>
html_builder::make_element_for_diagnostic (const diagnostic_info &diagnostic,
					   enum kind orig_diag_kind,
					   bool alert)
{
  const int diag_idx = m_next_diag_id++;
  std::string diag_id;
  {
    pretty_printer pp;
    pp_printf (&pp, "gcc-diag-%i", diag_idx);
    diag_id = pp_formatted_text (&pp);
  }

  // TODO: might be nice to emulate the text output format, but colorize it

  /* See https://pf3.patternfly.org/v3/pattern-library/widgets/#alerts
     which has this example:
<div class="alert alert-danger">
  <span class="pficon pficon-error-circle-o"></span>
  <strong>Hey there is a problem!</strong> Yeah this is really messed up and you should <a href="#" class="alert-link">know about it</a>.
</div>
  */
  auto diag_element = make_div ("gcc-diagnostic");
  diag_element->set_attr ("id", diag_id);
  if (alert)
    diag_element->set_attr ("class",
			    get_pf_class_for_alert_div (diagnostic.m_kind));

  xml::printer xp (*diag_element.get ());
  const size_t depth_within_alert_div = 1;

  gcc_assert (xp.get_num_open_tags () == depth_within_alert_div);

  if (alert)
    {
      xp.push_tag_with_class ("span",
			      get_pf_class_for_alert_icon (diagnostic.m_kind),
			      true);
      xp.add_text (" ");
      xp.pop_tag ("span");
    }

  // The rest goes in the <div>...
  gcc_assert (xp.get_num_open_tags () == depth_within_alert_div);

  xp.push_tag_with_class ("div", "gcc-message", true);
  std::string message_alert_id (diag_id + "-message");
  xp.set_attr ("id", message_alert_id);
  add_focus_id (message_alert_id);

  const size_t depth_within_message_div = depth_within_alert_div + 1;
  gcc_assert (xp.get_num_open_tags () == depth_within_message_div);

  // Severity e.g. "warning: "
  bool show_severity = true;
  if (!alert)
    show_severity = false;
  if (show_severity)
  {
    xp.push_tag ("strong");
    xp.add_text (_(get_text_for_kind (diagnostic.m_kind)));
    xp.pop_tag ("strong");
    xp.add_text (" ");
  }

  // Add the message itself:
  html_token_printer tok_printer (*xp.get_insertion_point ());
  m_printer->set_token_printer (&tok_printer);
  pp_output_formatted_text (m_printer, m_context.get_urlifier ());
  m_printer->set_token_printer (nullptr);
  pp_clear_output_area (m_printer);

  // Add any metadata as a suffix to the message
  if (diagnostic.m_metadata)
    if (auto e = make_element_for_metadata (*diagnostic.m_metadata))
      {
	xp.add_text (" ");
	xp.append (std::move (e));
      }

  // Add any option as a suffix to the message

  label_text option_text = label_text::take
    (m_context.make_option_name (diagnostic.m_option_id,
				 orig_diag_kind, diagnostic.m_kind));
  if (option_text.get ())
    {
      label_text option_url = label_text::take
	(m_context.make_option_url (diagnostic.m_option_id));

      xp.add_text (" ");
      auto option_span = make_span ("gcc-option");
      option_span->add_text ("[");
      {
	if (option_url.get ())
	  {
	    auto anchor = std::make_unique<xml::element> ("a", true);
	    anchor->set_attr ("href", option_url.get ());
	    anchor->add_text (option_text.get ());
	    option_span->add_child (std::move (anchor));
	  }
	else
	  option_span->add_text (option_text.get ());
	option_span->add_text ("]");
      }
      xp.append (std::move (option_span));
    }

  gcc_assert (xp.get_num_open_tags () == depth_within_message_div);

  xp.pop_tag ("div");

  gcc_assert (xp.get_num_open_tags () == depth_within_alert_div);

  /* Show any logical location.  */
  if (m_logical_loc_mgr)
    if (auto client_data_hooks = m_context.get_client_data_hooks ())
      if (auto logical_loc = client_data_hooks->get_current_logical_location ())
	if (logical_loc != m_last_logical_location)
	  {
	    enum logical_locations::kind kind
	      = m_logical_loc_mgr->get_kind (logical_loc);;
	    if (const char *label = get_label_for_logical_location_kind (kind))
	      if (const char *name_with_scope
		  = m_logical_loc_mgr->get_name_with_scope (logical_loc))
		add_labelled_value (xp, "logical-location",
				    label, name_with_scope, true);
	    m_last_logical_location = logical_loc;
	  }

  /* Show any physical location.  */
  const expanded_location s
    = diagnostic_expand_location (&diagnostic);
  if (s != m_last_expanded_location
      || alert)
    {
      if (s.file
	  && (s.file != m_last_expanded_location.file
	      || alert))
	add_labelled_value (xp, "file", "File", s.file, false);
      if (s.line)
	{
	  add_labelled_value (xp, "line", "Line", std::to_string (s.line), false);
	  column_policy col_policy (m_context);
	  int converted_column = col_policy.converted_column (s);
	  if (converted_column >= 0)
	    add_labelled_value (xp, "column", "Column",
				std::to_string (converted_column),
				false);
	}
      if (s.file)
	m_last_expanded_location = s;
    }

  /* Source (and fix-it hints).  */
  {
    // TODO: m_context.m_last_location should be moved into the sink
    location_t saved = m_context.m_last_location;
    m_context.m_last_location = m_last_location;
    m_context.maybe_show_locus_as_html
      (*diagnostic.m_richloc,
       m_context.get_source_printing_options (),
       diagnostic.m_kind,
       xp,
       nullptr,
       nullptr);
    m_context.m_last_location = saved;
    m_last_location = m_context.m_last_location;
  }

  gcc_assert (xp.get_num_open_tags () == depth_within_alert_div);

  /* Execution path.  */
  if (auto path = diagnostic.m_richloc->get_path ())
    {
      xp.push_tag ("div");
      xp.set_attr ("id", "execution-path");

      xp.push_tag ("label", true);
      const int num_events = path->num_events ();
      pretty_printer pp;
      pp_printf_n (&pp, num_events,
		   "Execution path with %i event",
		   "Execution path with %i events",
		   num_events);
      xp.add_text_from_pp (pp);
      xp.pop_tag ("label");

      std::string event_id_prefix (diag_id + "-event-");
      html_path_label_writer event_label_writer (xp, *this, *path,
						 event_id_prefix);

      source_print_policy dspp (m_context);
      print_path_as_html (xp, *path, m_context, &event_label_writer,
			  dspp);

      xp.pop_tag ("div");
    }

  gcc_assert (xp.get_num_open_tags () == depth_within_alert_div);

  // Try to display any per-diagnostic graphs
  if (diagnostic.m_metadata)
    if (auto ldg = diagnostic.m_metadata->get_lazy_digraphs ())
      {
	auto &digraphs = ldg->get_or_create ();
	for (auto &dg : digraphs)
	  add_graph (*dg, *xp.get_insertion_point ());
      }

  if (auto patch_element = make_element_for_patch (diagnostic))
    {
      xp.push_tag ("div");
      xp.set_attr ("id", "suggested-fix");
      xp.push_tag ("label", true);
      xp.add_text ("Suggested fix");
      xp.pop_tag ("label");
      xp.append (std::move (patch_element));
      xp.pop_tag ("div");
    }

  return diag_element;
}

std::unique_ptr<xml::element>
html_builder::make_element_for_patch (const diagnostic_info &diagnostic)
{
  changes::change_set edit (m_context.get_file_cache ());
  edit.add_fixits (diagnostic.m_richloc);
  if (char *diff = edit.generate_diff (true))
    {
      if (strlen (diff) > 0)
	{
	  auto element = std::make_unique<xml::element> ("pre", true);
	  element->set_attr ("class", "gcc-generated-patch");
	  element->add_text (diff);
	  free (diff);
	  return element;
	}
      else
	free (diff);
    }
  return nullptr;
}

std::unique_ptr<xml::element>
html_builder::make_metadata_element (label_text label,
				     label_text url)
{
  auto item = make_span ("gcc-metadata-item");
  xml::printer xp (*item.get ());
  xp.add_text ("[");
  {
    if (url.get ())
      {
	xp.push_tag ("a", true);
	xp.set_attr ("href", url.get ());
      }
    xp.add_text (label.get ());
    if (url.get ())
      xp.pop_tag ("a");
  }
  xp.add_text ("]");
  return item;
}

std::unique_ptr<xml::element>
html_builder::make_element_for_metadata (const metadata &m)
{
  auto span_metadata = make_span ("gcc-metadata");

  int cwe = m.get_cwe ();
  if (cwe)
    {
      pretty_printer pp;
      pp_printf (&pp, "CWE-%i", cwe);
      label_text label = label_text::take (xstrdup (pp_formatted_text (&pp)));
      label_text url = label_text::take (get_cwe_url (cwe));
      span_metadata->add_child
	(make_metadata_element (std::move (label), std::move (url)));
    }

  for (unsigned idx = 0; idx < m.get_num_rules (); ++idx)
    {
      auto &rule = m.get_rule (idx);
      label_text label = label_text::take (rule.make_description ());
      label_text url = label_text::take (rule.make_url ());
      span_metadata->add_child
	(make_metadata_element (std::move (label), std::move (url)));
    }

  if (span_metadata->m_children.empty ())
    return nullptr;

  return span_metadata;
}

/* Implementation of diagnostics::context::m_diagrams.m_emission_cb
   for HTML output.  */

void
html_builder::emit_diagram (const diagram &)
{
  /* We must be within the emission of a top-level diagnostic.  */
  gcc_assert (m_cur_diagnostic_element);

  // TODO: currently a no-op
}

void
html_builder::add_graph (const digraphs::digraph &dg,
			 xml::element &parent_element)
{
  auto div = std::make_unique<xml::element> ("div", false);
  div->set_attr ("class", "gcc-directed-graph");
  xml::printer xp (*div);

  if (m_html_gen_opts.m_show_graph_sarif)
    {
      // For debugging, show the SARIF src inline:
      pretty_printer pp;
      dg.make_json_sarif_graph ()->print (&pp, true);
      print_pre_source (xp, pp_formatted_text (&pp));
    }

  if (auto dot_graph = dg.make_dot_graph ())
    {
      if (m_html_gen_opts.m_show_graph_dot_src)
	{
	  // For debugging, show the dot src inline:
	  pretty_printer pp;
	  dot::writer w (pp);
	  dot_graph->print (w);
	  print_pre_source (xp, pp_formatted_text (&pp));
	}

      if (auto svg_element = dot::make_svg_from_graph (*dot_graph))
	{
	  if (const char *description = dg.get_description ())
	    {
	      xp.push_tag ("h2", true);
	      xp.add_text (description);
	      xp.pop_tag ("h2");
	    }
	  xp.append (std::move (svg_element));
	  parent_element.add_child (std::move (div));
	}
    }
}

void
html_builder::emit_global_graph (const lazily_created<digraphs::digraph> &ldg)
{
  auto &dg = ldg.get_or_create ();
  gcc_assert (m_body_element);
  add_graph (dg, *m_body_element);
}

void
html_builder::
add_graph_for_logical_loc (const lazily_created<digraphs::digraph> &ldg,
			   logical_locations::key logical_loc)
{
  gcc_assert (m_body_element);

  auto iter = m_per_logical_loc_graphs.find (logical_loc);
  if (iter == m_per_logical_loc_graphs.end ())
    {
      auto logical_loc_element = make_div ("gcc-logical-location");
      iter = m_per_logical_loc_graphs.insert ({logical_loc,
	  logical_loc_element.get ()}).first;
      m_body_element->add_child (std::move (logical_loc_element));
    }

  auto &dg = ldg.get_or_create ();
  add_graph (dg, *iter->second);
}

/* Implementation of "end_group_cb" for HTML output.  */

void
html_builder::end_group ()
{
  if (m_cur_diagnostic_element)
    m_diagnostics_element->add_child (std::move (m_cur_diagnostic_element));
}

/* Create a top-level object, and add it to all the results
   (and other entities) we've seen so far.

   Flush it all to OUTF.  */

void
html_builder::flush_to_file (FILE *outf)
{
  DIAGNOSTICS_LOG_SCOPE_PRINTF0 (m_context.get_logger (),
				 "diagnostics::html_builder::flush_to_file");
  if (m_html_gen_opts.m_javascript)
    {
      gcc_assert (m_head_element);
      xml::printer xp (*m_head_element);
      /* Add an initialization of the global js variable "focus_ids"
	 using the array of IDs we saved as we went.  */
      xp.push_tag ("script");
      pretty_printer pp;
      pp_string (&pp, "focus_ids = ");
      m_ui_focus_ids.print (&pp, true);
      pp_string (&pp, ";\n");
      xp.add_raw (pp_formatted_text (&pp));
      xp.pop_tag ("script");
    }
  auto top = m_document.get ();
  top->dump (outf);
  fprintf (outf, "\n");
}

class html_sink : public sink
{
public:
  ~html_sink ()
  {
    /* Any diagnostics should have been handled by now.
       If not, then something's gone wrong with diagnostic
       groupings.  */
    std::unique_ptr<xml::element> pending_diag
      = m_builder.take_current_diagnostic ();
    gcc_assert (!pending_diag);
  }

  void dump (FILE *out, int indent) const override
  {
    sink::dump (out, indent);
    dumping::emit_heading (out, indent, "html_builder");
    m_builder.dump (out, indent + 2);
  }

  void
  set_main_input_filename (const char *name) final override
  {
    m_builder.set_main_input_filename (name);
  }

  std::unique_ptr<per_sink_buffer>
  make_per_sink_buffer () final override
  {
    return std::make_unique<html_sink_buffer> (m_builder);
  }
  void set_buffer (per_sink_buffer *base_buffer) final override
  {
    html_sink_buffer *buffer
      = static_cast<html_sink_buffer *> (base_buffer);
    m_buffer = buffer;
  }

  void on_begin_group () final override
  {
    /* No-op,  */
  }
  void on_end_group () final override
  {
    m_builder.end_group ();
  }
  void
  on_report_diagnostic (const diagnostic_info &diagnostic,
			enum kind orig_diag_kind) final override
  {
    DIAGNOSTICS_LOG_SCOPE_PRINTF0
      (get_logger (),
       "diagnostics::html_sink::on_report_diagnostic");
    m_builder.on_report_diagnostic (diagnostic, orig_diag_kind, m_buffer);
  }
  void on_diagram (const diagram &d) final override
  {
    m_builder.emit_diagram (d);
  }
  void after_diagnostic (const diagnostic_info &) final override
  {
    /* No-op, but perhaps could show paths here.  */
  }
  bool follows_reference_printer_p () const final override
  {
    return false;
  }
  void update_printer () final override
  {
    m_printer = m_context.clone_printer ();

    /* Don't colorize the text.  */
    pp_show_color (m_printer.get ()) = false;

    /* No textual URLs.  */
    m_printer->set_url_format (URL_FORMAT_NONE);

    /* Update the builder to use the new printer.  */
    m_builder.set_printer (*get_printer ());
  }

  void
  report_global_digraph (const lazily_created<digraphs::digraph> &ldg)
    final override
  {
    m_builder.emit_global_graph (ldg);
  }

  void
  report_digraph_for_logical_location (const lazily_created<digraphs::digraph> &ldg,
				       logical_locations::key logical_loc) final override
  {
    m_builder.add_graph_for_logical_loc (ldg, logical_loc);
  }

  const xml::document &get_document () const
  {
    return m_builder.get_document ();
  }

  html_builder &get_builder () { return m_builder; }

protected:
  html_sink (context &dc,
	     const line_maps *line_maps,
	     const html_generation_options &html_gen_opts)
  : sink (dc),
    m_builder (dc, *get_printer (), line_maps, html_gen_opts),
    m_buffer (nullptr)
  {}

  html_builder m_builder;
  html_sink_buffer *m_buffer;
};

class html_file_sink : public html_sink
{
public:
  html_file_sink (context &dc,
		  const line_maps *line_maps,
		  const html_generation_options &html_gen_opts,
		  output_file output_file_)
  : html_sink (dc, line_maps, html_gen_opts),
    m_output_file (std::move (output_file_))
  {
    gcc_assert (m_output_file.get_open_file ());
    gcc_assert (m_output_file.get_filename ());
  }
  ~html_file_sink ()
  {
    m_builder.flush_to_file (m_output_file.get_open_file ());
  }
  void dump_kind (FILE *out) const override
  {
    fprintf (out, "html_file_sink: %s",
	     m_output_file.get_filename ());
  }
  bool machine_readable_stderr_p () const final override
  {
    return false;
  }

private:
  output_file m_output_file;
};

/* Attempt to open BASE_FILE_NAME.html for writing.
   Return a non-null output_file,
   or return a null output_file and complain to DC
   using LINE_MAPS.  */

output_file
open_html_output_file (context &dc,
		       line_maps *line_maps,
		       const char *base_file_name)
{
  if (!base_file_name)
    {
      rich_location richloc (line_maps, UNKNOWN_LOCATION);
      dc.emit_diagnostic_with_group
	(kind::error, richloc, nullptr, 0,
	 "unable to determine filename for HTML output");
      return output_file ();
    }

  label_text filename = label_text::take (concat (base_file_name,
						  ".html",
						  nullptr));
  FILE *outf = fopen (filename.get (), "w");
  if (!outf)
    {
      rich_location richloc (line_maps, UNKNOWN_LOCATION);
      dc.emit_diagnostic_with_group
	(kind::error, richloc, nullptr, 0,
	 "unable to open %qs for HTML output: %m",
	 filename.get ());
      return output_file ();
    }
  return output_file (outf, true, std::move (filename));
}

std::unique_ptr<sink>
make_html_sink (context &dc,
		const line_maps &line_maps,
		const html_generation_options &html_gen_opts,
		output_file output_file_)
{
  auto sink
    = std::make_unique<html_file_sink> (dc,
					&line_maps,
					html_gen_opts,
					std::move (output_file_));
  sink->update_printer ();
  return sink;
}

#if CHECKING_P

namespace selftest {

/* Helper for writing tests of html_token_printer.
   Printing to m_pp will appear as HTML within m_top_element, a <div>.  */

struct token_printer_test
{
  token_printer_test ()
  : m_top_element ("div", true),
    m_tok_printer (m_top_element)
  {
    m_pp.set_token_printer (&m_tok_printer);
  }

  xml::element m_top_element;
  html_token_printer m_tok_printer;
  pretty_printer m_pp;
};

static void
test_token_printer ()
{
  {
    token_printer_test t;
    pp_printf (&t.m_pp, "hello world");
    ASSERT_XML_PRINT_EQ
      (t.m_top_element,
       "<div>hello world</div>\n");
  }

  {
    token_printer_test t;
    pp_printf (&t.m_pp, "%qs: %qs", "foo", "bar");
    ASSERT_XML_PRINT_EQ
      (t.m_top_element,
       "<div>"
       "`"
       "<span class=\"gcc-quoted-text\">"
       "foo"
       "</span>"
       "&apos;: `"
       "<span class=\"gcc-quoted-text\">"
       "bar"
       "</span>"
       "&apos;"
       "</div>\n");
  }

  {
    token_printer_test t;
    paths::event_id_t event_id (0);
    pp_printf (&t.m_pp, "foo %@ bar", &event_id);
    ASSERT_XML_PRINT_EQ
      (t.m_top_element,
       "<div>foo (1) bar</div>\n");
  }
}

/* A subclass of html_sink for writing selftests.
   The XML output is cached internally, rather than written
   out to a file.  */

class test_html_context : public test_context
{
public:
  test_html_context ()
  {
    html_generation_options html_gen_opts;
    html_gen_opts.m_css = false;
    html_gen_opts.m_javascript = false;
    auto sink = std::make_unique<html_buffered_sink> (*this,
						      line_table,
						      html_gen_opts);
    sink->update_printer ();
    sink->set_main_input_filename ("(main input filename)");
    m_format = sink.get (); // borrowed

    set_sink (std::move (sink));
  }

  const xml::document &get_document () const
  {
    return m_format->get_document ();
  }

  html_builder &get_builder () const
  {
    return m_format->get_builder ();
  }

private:
  class html_buffered_sink : public html_sink
  {
  public:
    html_buffered_sink (context &dc,
			const line_maps *line_maps,
			const html_generation_options &html_gen_opts)
    : html_sink (dc, line_maps, html_gen_opts)
    {
    }
    void dump_kind (FILE *out) const final override
    {
      fprintf (out, "html_buffered_sink");
    }
    bool machine_readable_stderr_p () const final override
    {
      return true;
    }
  };

  html_sink *m_format; // borrowed
};

/* Test of reporting a diagnostic at UNKNOWN_LOCATION to a
   diagnostics::context and examining the generated XML document.
   Verify various basic properties. */

static void
test_simple_log ()
{
  test_html_context dc;

  rich_location richloc (line_table, UNKNOWN_LOCATION);
  dc.report (kind::error, richloc, nullptr, 0, "this is a test: %qs", "foo");

  const xml::document &doc  = dc.get_document ();

  ASSERT_XML_PRINT_EQ
    (doc,
     ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<!DOCTYPE html\n"
      "     PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
      "     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
      "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
      "  <head>\n"
      "    <title>(main input filename)</title>\n"
      "  </head>\n"
      "  <body>\n"
      "    <div class=\"gcc-diagnostic-list\">\n"
      "      <div class=\"alert alert-danger\" id=\"gcc-diag-0\">\n"
      "        <span class=\"pficon pficon-error-circle-o\"> </span>\n"
      "        <div class=\"gcc-message\" id=\"gcc-diag-0-message\"><strong>error: </strong> this is a test: `<span class=\"gcc-quoted-text\">foo</span>&apos;</div>\n"
      "      </div>\n"
      "    </div>\n"
      "  </body>\n"
      "</html>\n"));
}

static void
test_metadata ()
{
  test_html_context dc;
  html_builder &b = dc.get_builder ();

  {
    metadata m;
    m.add_cwe (415);
    auto element = b.make_element_for_metadata (m);
    ASSERT_XML_PRINT_EQ
      (*element,
       "<span class=\"gcc-metadata\">"
       "<span class=\"gcc-metadata-item\">"
       "["
       "<a href=\"https://cwe.mitre.org/data/definitions/415.html\">"
       "CWE-415"
       "</a>"
       "]"
       "</span>"
       "</span>\n");
  }

  {
    metadata m;
    metadata::precanned_rule rule ("MISC-42",
				   "http://example.com");
    m.add_rule (rule);
    auto element = b.make_element_for_metadata (m);
    ASSERT_XML_PRINT_EQ
      (*element,
       "<span class=\"gcc-metadata\">"
       "<span class=\"gcc-metadata-item\">"
       "["
       "<a href=\"http://example.com\">"
       "MISC-42"
       "</a>"
       "]"
       "</span>"
       "</span>\n");
  }
}

/* Run all of the selftests within this file.  */

void
html_sink_cc_tests ()
{
  ::selftest::auto_fix_quotes fix_quotes;
  test_token_printer ();
  test_simple_log ();
  test_metadata ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace diagnostics
