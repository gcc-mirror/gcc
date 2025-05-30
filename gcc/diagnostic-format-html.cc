/* HTML output for diagnostics.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
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
#include "diagnostic.h"
#include "diagnostic-metadata.h"
#include "diagnostic-format.h"
#include "diagnostic-format-html.h"
#include "diagnostic-format-text.h"
#include "diagnostic-output-file.h"
#include "diagnostic-buffer.h"
#include "selftest.h"
#include "selftest-diagnostic.h"
#include "pretty-print-format-impl.h"
#include "pretty-print-urlifier.h"
#include "edit-context.h"
#include "intl.h"
#include "xml.h"
#include "xml-printer.h"
#include "json.h"

// struct html_generation_options

html_generation_options::html_generation_options ()
: m_css (true),
  m_javascript (true)
{
}

namespace xml {

/* Disable warnings about quoting issues in the pp_xxx calls below
   that (intentionally) don't follow GCC diagnostic conventions.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif


/* Implementation.  */

static void
write_escaped_text (pretty_printer *pp, const char *text)
{
  gcc_assert (text);

  for (const char *p = text; *p; ++p)
    {
      char ch = *p;
      switch (ch)
	{
	default:
	  pp_character (pp, ch);
	  break;
	case '\'':
	  pp_string (pp, "&apos;");
	  break;
	case '"':
	  pp_string (pp, "&quot;");
	  break;
	case '&':
	  pp_string (pp, "&amp;");
	  break;
	case '<':
	  pp_string (pp, "&lt;");
	  break;
	case '>':
	  pp_string (pp, "&gt;");
	  break;
	}
    }
}

/* struct node.  */

void
node::dump (FILE *out) const
{
  pretty_printer pp;
  pp.set_output_stream (out);
  write_as_xml (&pp, 0, true);
  pp_flush (&pp);
}

/* struct text : public node.  */

void
text::write_as_xml (pretty_printer *pp, int depth, bool indent) const
{
  if (indent)
    {
      for (int i = 0; i < depth; ++i)
	pp_string (pp, "  ");
    }
  write_escaped_text (pp, m_str.c_str ());
  if (indent)
    pp_newline (pp);
}

/* struct node_with_children : public node.  */

void
node_with_children::add_child (std::unique_ptr<node> node)
{
  gcc_assert (node.get ());
  m_children.push_back (std::move (node));
}

void
node_with_children::add_text (std::string str)
{
  // Consolidate runs of text
  if (!m_children.empty ())
    if (text *t = m_children.back ()->dyn_cast_text ())
      {
	t->m_str += std::move (str);
	return;
      }
  add_child (std::make_unique <text> (std::move (str)));
}


/* struct document : public node_with_children.  */

void
document::write_as_xml (pretty_printer *pp, int depth, bool indent) const
{
  pp_string (pp, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
  pp_string (pp, "<!DOCTYPE html\n"
	     "     PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
	     "     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">");
  if (indent)
    pp_newline (pp);
  for (auto &iter : m_children)
    iter->write_as_xml (pp, depth, indent);
}

/* struct element : public node_with_children.  */

void
element::write_as_xml (pretty_printer *pp, int depth, bool indent) const
{
  if (indent)
    {
      for (int i = 0; i < depth; ++i)
	pp_string (pp, "  ");
    }

  pp_printf (pp, "<%s", m_kind.c_str ());
  for (auto &key : m_key_insertion_order)
    {
      auto iter = m_attributes.find (key);
      if (iter != m_attributes.end ())
	{
	  pp_printf (pp, " %s=\"", key.c_str ());
	  write_escaped_text (pp, iter->second.c_str ());
	  pp_string (pp, "\"");
	}
    }
  if (m_children.empty ())
    pp_string (pp, "/>");
  else
    {
      const bool indent_children = m_preserve_whitespace ? false : indent;
      pp_string (pp, ">");
      if (indent_children)
	pp_newline (pp);
      for (auto &child : m_children)
	child->write_as_xml (pp, depth + 1, indent_children);
      if (indent_children)
	{
	  for (int i = 0; i < depth; ++i)
	    pp_string (pp, "  ");
	}
      pp_printf (pp, "</%s>", m_kind.c_str ());
    }

  if (indent)
    pp_newline (pp);
}

void
element::set_attr (const char *name, std::string value)
{
  auto iter = m_attributes.find (name);
  if (iter == m_attributes.end ())
    m_key_insertion_order.push_back (name);
  m_attributes[name] = std::move (value);
}

// struct raw : public node

void
raw::write_as_xml (pretty_printer *pp,
		   int /*depth*/, bool /*indent*/) const
{
  pp_string (pp, m_xml_src.c_str ());
}

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif

// class printer

printer::printer (element &insertion_point)
{
  m_open_tags.push_back (&insertion_point);
}

void
printer::push_tag (std::string name,
		   bool preserve_whitespace)
{
  push_element
    (std::make_unique<element> (std::move (name),
				preserve_whitespace));
}

void
printer::push_tag_with_class (std::string name, std::string class_,
			      bool preserve_whitespace)
{
  auto new_element
    = std::make_unique<element> (std::move (name),
				 preserve_whitespace);
  new_element->set_attr ("class", class_);
  push_element (std::move (new_element));
}

void
printer::pop_tag ()
{
  m_open_tags.pop_back ();
}

void
printer::set_attr (const char *name, std::string value)
{
  m_open_tags.back ()->set_attr (name, value);
}

void
printer::add_text (std::string text)
{
  element *parent = m_open_tags.back ();
  parent->add_text (std::move (text));
}

void
printer::add_raw (std::string text)
{
  element *parent = m_open_tags.back ();
  parent->add_child (std::make_unique<xml::raw> (std::move (text)));
}

void
printer::push_element (std::unique_ptr<element> new_element)
{
  element *parent = m_open_tags.back ();
  m_open_tags.push_back (new_element.get ());
  parent->add_child (std::move (new_element));
}

void
printer::append (std::unique_ptr<node> new_node)
{
  element *parent = m_open_tags.back ();
  parent->add_child (std::move (new_node));
}

element *
printer::get_insertion_point () const
{
  return m_open_tags.back ();
}

} // namespace xml

class html_builder;

/* Concrete buffering implementation subclass for HTML output.  */

class diagnostic_html_format_buffer : public diagnostic_per_format_buffer
{
public:
  friend class html_builder;
  friend class html_output_format;

  diagnostic_html_format_buffer (html_builder &builder)
  : m_builder (builder)
  {}

  void dump (FILE *out, int indent) const final override;
  bool empty_p () const final override;
  void move_to (diagnostic_per_format_buffer &dest) final override;
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
  friend class diagnostic_html_format_buffer;

  html_builder (diagnostic_context &context,
		pretty_printer &pp,
		const line_maps *line_maps,
		const html_generation_options &html_gen_opts);

  void on_report_diagnostic (const diagnostic_info &diagnostic,
			     diagnostic_t orig_diag_kind,
			     diagnostic_html_format_buffer *buffer);
  void emit_diagram (const diagnostic_diagram &diagram);
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
  make_element_for_metadata (const diagnostic_metadata &metadata);

  std::unique_ptr<xml::element>
  make_element_for_patch (const diagnostic_info &diagnostic);

  void add_focus_id (std::string focus_id)
  {
    m_ui_focus_ids.append_string (focus_id.c_str ());
  }

private:
  std::unique_ptr<xml::element>
  make_element_for_diagnostic (const diagnostic_info &diagnostic,
			       diagnostic_t orig_diag_kind);

  std::unique_ptr<xml::element>
  make_metadata_element (label_text label,
			 label_text url);

  diagnostic_context &m_context;
  pretty_printer *m_printer;
  const line_maps *m_line_maps;
  html_generation_options m_html_gen_opts;

  std::unique_ptr<xml::document> m_document;
  xml::element *m_head_element;
  xml::element *m_diagnostics_element;
  std::unique_ptr<xml::element> m_cur_diagnostic_element;
  int m_next_diag_id; // for handing out unique IDs
  json::array m_ui_focus_ids;
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

/* class diagnostic_html_format_buffer : public diagnostic_per_format_buffer.  */

void
diagnostic_html_format_buffer::dump (FILE *out, int indent) const
{
  fprintf (out, "%*sdiagnostic_html_format_buffer:\n", indent, "");
  int idx = 0;
  for (auto &result : m_results)
    {
      fprintf (out, "%*sresult[%i]:\n", indent + 2, "", idx);
      result->dump (out);
      fprintf (out, "\n");
      ++idx;
    }
}

bool
diagnostic_html_format_buffer::empty_p () const
{
  return m_results.empty ();
}

void
diagnostic_html_format_buffer::move_to (diagnostic_per_format_buffer &base)
{
  diagnostic_html_format_buffer &dest
    = static_cast<diagnostic_html_format_buffer &> (base);
  for (auto &&result : m_results)
    dest.m_results.push_back (std::move (result));
  m_results.clear ();
}

void
diagnostic_html_format_buffer::clear ()
{
  m_results.clear ();
}

void
diagnostic_html_format_buffer::flush ()
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
     "              white-space: pre; }\n"
     "    .annotation { color: green;\n"
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
     "  function unhighlight_current_focus_idx ()\n"
     "  {\n"
     "      get_focus_span (current_focus_idx).classList.remove ('selected');\n"
     "  }\n"
     "  function highlight_current_focus_idx ()\n"
     "  {\n"
     "      const el = get_focus_span (current_focus_idx);\n"
     "      el.classList.add ('selected');\n"
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

/* html_builder's ctor.  */

html_builder::html_builder (diagnostic_context &context,
			    pretty_printer &pp,
			    const line_maps *line_maps,
			    const html_generation_options &html_gen_opts)
: m_context (context),
  m_printer (&pp),
  m_line_maps (line_maps),
  m_html_gen_opts (html_gen_opts),
  m_head_element (nullptr),
  m_diagnostics_element (nullptr),
  m_next_diag_id (0)
{
  gcc_assert (m_line_maps);

  m_document = std::make_unique<xml::document> ();
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
	xp.add_text ("Title goes here");
      }
      if (m_html_gen_opts.m_css)
	xp.add_raw (HTML_STYLE);
      if (m_html_gen_opts.m_javascript)
	{
	  xp.push_tag ("script");
	  /* Escaping rules are different for HTML <script> elements,
	     so add the script "raw" for now.  */
	  xp.add_raw (HTML_SCRIPT);
	  xp.pop_tag (); // script
	}
    }

    {
      xml::auto_print_element body (xp, "body");
      {
	auto diagnostics_element = make_div ("gcc-diagnostic-list");
	m_diagnostics_element = diagnostics_element.get ();
	xp.append (std::move (diagnostics_element));
      }
    }
  }
}

/* Implementation of "on_report_diagnostic" for HTML output.  */

void
html_builder::on_report_diagnostic (const diagnostic_info &diagnostic,
				    diagnostic_t orig_diag_kind,
				    diagnostic_html_format_buffer *buffer)
{
  if (diagnostic.kind == DK_ICE || diagnostic.kind == DK_ICE_NOBT)
    {
      /* Print a header for the remaining output to stderr, and
	 return, attempting to print the usual ICE messages to
	 stderr.  Hopefully this will be helpful to the user in
	 indicating what's gone wrong (also for DejaGnu, for pruning
	 those messages).   */
      fnotice (stderr, "Internal compiler error:\n");
    }

  auto diag_element
    = make_element_for_diagnostic (diagnostic, orig_diag_kind);
  if (buffer)
    {
      gcc_assert (!m_cur_diagnostic_element);
      buffer->m_results.push_back (std::move (diag_element));
    }
  else
    {
      if (m_cur_diagnostic_element)
	/* Nested diagnostic.  */
	m_cur_diagnostic_element->add_child (std::move (diag_element));
      else
	/* Top-level diagnostic.  */
	m_cur_diagnostic_element = std::move (diag_element);
    }
}

/* Custom subclass of html_label_writer.
   Wrap labels within a <span> element, supplying them with event IDs.
   Add the IDs to the list of focus IDs.  */

class html_path_label_writer : public html_label_writer
{
public:
  html_path_label_writer (xml::printer &xp,
			  html_builder &builder,
			  const std::string &event_id_prefix)
  : m_xp (xp),
    m_html_builder (builder),
    m_event_id_prefix (event_id_prefix),
    m_next_event_idx (0)
  {
  }

  void begin_label () final override
  {
    m_xp.push_tag_with_class ("span", "event", true);
    pretty_printer pp;
    pp_printf (&pp, "%s%i",
	       m_event_id_prefix.c_str (), m_next_event_idx++);
    m_xp.set_attr ("id", pp_formatted_text (&pp));
    m_html_builder.add_focus_id (pp_formatted_text (&pp));
  }

  void end_label () final override
  {
    m_xp.pop_tag (); // span
  }

private:
  xml::printer &m_xp;
  html_builder &m_html_builder;
  const std::string &m_event_id_prefix;
  int m_next_event_idx;
};

std::unique_ptr<xml::element>
html_builder::make_element_for_diagnostic (const diagnostic_info &diagnostic,
					   diagnostic_t orig_diag_kind)
{
  class html_token_printer : public token_printer
  {
  public:
    html_token_printer (xml::printer &xp)
    : m_xp (xp)
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
	    m_xp.pop_tag ();
	    break;

	  case pp_token::kind::begin_quote:
	    {
	      m_xp.add_text (open_quote);
	      m_xp.push_tag_with_class ("span", "gcc-quoted-text");
	    }
	    break;
	  case pp_token::kind::end_quote:
	    {
	      m_xp.pop_tag ();
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
	    m_xp.pop_tag ();
	    break;
	  }
    }

  private:
    xml::printer &m_xp;
  };

  auto diag_element = make_div ("gcc-diagnostic");

  const int diag_idx = m_next_diag_id++;
  std::string diag_id;
  {
    pretty_printer pp;
    pp_printf (&pp, "gcc-diag-%i", diag_idx);
    diag_id = pp_formatted_text (&pp);
  }
  diag_element->set_attr ("id", diag_id);

  // TODO: might be nice to emulate the text output format, but colorize it

  auto message_span = make_span ("gcc-message");
  std::string message_span_id (diag_id + "-message");
  message_span->set_attr ("id", message_span_id);
  add_focus_id (message_span_id);

  xml::printer xp (*message_span.get ());
  html_token_printer tok_printer (xp);
  m_printer->set_token_printer (&tok_printer);
  pp_output_formatted_text (m_printer, m_context.get_urlifier ());
  m_printer->set_token_printer (nullptr);
  pp_clear_output_area (m_printer);
  diag_element->add_child (std::move (message_span));

  if (diagnostic.metadata)
    {
      diag_element->add_text (" ");
      diag_element->add_child
	(make_element_for_metadata (*diagnostic.metadata));
    }

  label_text option_text = label_text::take
    (m_context.make_option_name (diagnostic.option_id,
				 orig_diag_kind, diagnostic.kind));
  if (option_text.get ())
    {
      label_text option_url = label_text::take
	(m_context.make_option_url (diagnostic.option_id));

      diag_element->add_text (" ");
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
      diag_element->add_child (std::move (option_span));
    }

  /* Source (and fix-it hints).  */
  {
    xml::printer xp (*diag_element);
    m_context.m_last_location = UNKNOWN_LOCATION;
    m_context.maybe_show_locus_as_html (*diagnostic.richloc,
					m_context.m_source_printing,
					diagnostic.kind,
					xp,
					nullptr,
					nullptr);
  }

  /* Execution path.  */
  if (auto path = diagnostic.richloc->get_path ())
    {
      xml::printer xp (*diag_element);
      std::string event_id_prefix (diag_id + "-event-");
      html_path_label_writer event_label_writer (xp, *this,
						 event_id_prefix);
      diagnostic_source_print_policy dspp (m_context);
      print_path_as_html (xp, *path, m_context, &event_label_writer,
			  dspp);
    }

  if (auto patch_element = make_element_for_patch (diagnostic))
    diag_element->add_child (std::move (patch_element));

  return diag_element;
}

std::unique_ptr<xml::element>
html_builder::make_element_for_patch (const diagnostic_info &diagnostic)
{
  edit_context ec (m_context.get_file_cache ());
  ec.add_fixits (diagnostic.richloc);
  if (char *diff = ec.generate_diff (true))
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
      xp.pop_tag ();
  }
  xp.add_text ("]");
  return item;
}

std::unique_ptr<xml::element>
html_builder::make_element_for_metadata (const diagnostic_metadata &metadata)
{
  auto span_metadata = make_span ("gcc-metadata");

  int cwe = metadata.get_cwe ();
  if (cwe)
    {
      pretty_printer pp;
      pp_printf (&pp, "CWE-%i", cwe);
      label_text label = label_text::take (xstrdup (pp_formatted_text (&pp)));
      label_text url = label_text::take (get_cwe_url (cwe));
      span_metadata->add_child
	(make_metadata_element (std::move (label), std::move (url)));
    }

  for (unsigned idx = 0; idx < metadata.get_num_rules (); ++idx)
    {
      auto &rule = metadata.get_rule (idx);
      label_text label = label_text::take (rule.make_description ());
      label_text url = label_text::take (rule.make_url ());
      span_metadata->add_child
	(make_metadata_element (std::move (label), std::move (url)));
    }

  return span_metadata;
}

/* Implementation of diagnostic_context::m_diagrams.m_emission_cb
   for HTML output.  */

void
html_builder::emit_diagram (const diagnostic_diagram &/*diagram*/)
{
  /* We must be within the emission of a top-level diagnostic.  */
  gcc_assert (m_cur_diagnostic_element);

  // TODO: currently a no-op
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
      xp.pop_tag (); // script
    }
  auto top = m_document.get ();
  top->dump (outf);
  fprintf (outf, "\n");
}

class html_output_format : public diagnostic_output_format
{
public:
  ~html_output_format ()
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
    fprintf (out, "%*shtml_output_format\n", indent, "");
    diagnostic_output_format::dump (out, indent);
  }

  std::unique_ptr<diagnostic_per_format_buffer>
  make_per_format_buffer () final override
  {
    return std::make_unique<diagnostic_html_format_buffer> (m_builder);
  }
  void set_buffer (diagnostic_per_format_buffer *base_buffer) final override
  {
    diagnostic_html_format_buffer *buffer
      = static_cast<diagnostic_html_format_buffer *> (base_buffer);
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
			diagnostic_t orig_diag_kind) final override
  {
    m_builder.on_report_diagnostic (diagnostic, orig_diag_kind, m_buffer);
  }
  void on_diagram (const diagnostic_diagram &diagram) final override
  {
    m_builder.emit_diagram (diagram);
  }
  void after_diagnostic (const diagnostic_info &)
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

  const xml::document &get_document () const
  {
    return m_builder.get_document ();
  }

  html_builder &get_builder () { return m_builder; }

protected:
  html_output_format (diagnostic_context &context,
		      const line_maps *line_maps,
		      const html_generation_options &html_gen_opts)
  : diagnostic_output_format (context),
    m_builder (context, *get_printer (), line_maps, html_gen_opts),
    m_buffer (nullptr)
  {}

  html_builder m_builder;
  diagnostic_html_format_buffer *m_buffer;
};

class html_file_output_format : public html_output_format
{
public:
  html_file_output_format (diagnostic_context &context,
			   const line_maps *line_maps,
			   const html_generation_options &html_gen_opts,
			   diagnostic_output_file output_file)
  : html_output_format (context, line_maps, html_gen_opts),
    m_output_file (std::move (output_file))
  {
    gcc_assert (m_output_file.get_open_file ());
    gcc_assert (m_output_file.get_filename ());
  }
  ~html_file_output_format ()
  {
    m_builder.flush_to_file (m_output_file.get_open_file ());
  }
  void dump (FILE *out, int indent) const override
  {
    fprintf (out, "%*shtml_file_output_format: %s\n",
	     indent, "",
	     m_output_file.get_filename ());
    diagnostic_output_format::dump (out, indent);
  }
  bool machine_readable_stderr_p () const final override
  {
    return false;
  }

private:
  diagnostic_output_file m_output_file;
};

/* Attempt to open BASE_FILE_NAME.html for writing.
   Return a non-null diagnostic_output_file,
   or return a null diagnostic_output_file and complain to CONTEXT
   using LINE_MAPS.  */

diagnostic_output_file
diagnostic_output_format_open_html_file (diagnostic_context &context,
					 line_maps *line_maps,
					 const char *base_file_name)
{
  if (!base_file_name)
    {
      rich_location richloc (line_maps, UNKNOWN_LOCATION);
      context.emit_diagnostic_with_group
	(DK_ERROR, richloc, nullptr, 0,
	 "unable to determine filename for HTML output");
      return diagnostic_output_file ();
    }

  label_text filename = label_text::take (concat (base_file_name,
						  ".html",
						  nullptr));
  FILE *outf = fopen (filename.get (), "w");
  if (!outf)
    {
      rich_location richloc (line_maps, UNKNOWN_LOCATION);
      context.emit_diagnostic_with_group
	(DK_ERROR, richloc, nullptr, 0,
	 "unable to open %qs for HTML output: %m",
	 filename.get ());
      return diagnostic_output_file ();
    }
  return diagnostic_output_file (outf, true, std::move (filename));
}

std::unique_ptr<diagnostic_output_format>
make_html_sink (diagnostic_context &context,
		const line_maps &line_maps,
		const html_generation_options &html_gen_opts,
		diagnostic_output_file output_file)
{
  auto sink
    = std::make_unique<html_file_output_format> (context,
						 &line_maps,
						 html_gen_opts,
						 std::move (output_file));
  sink->update_printer ();
  return sink;
}

#if CHECKING_P

namespace selftest {

/* A subclass of html_output_format for writing selftests.
   The XML output is cached internally, rather than written
   out to a file.  */

class test_html_diagnostic_context : public test_diagnostic_context
{
public:
  test_html_diagnostic_context ()
  {
    html_generation_options html_gen_opts;
    html_gen_opts.m_css = false;
    html_gen_opts.m_javascript = false;
    auto sink = std::make_unique<html_buffered_output_format> (*this,
							       line_table,
							       html_gen_opts);
    sink->update_printer ();
    m_format = sink.get (); // borrowed

    set_output_format (std::move (sink));
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
  class html_buffered_output_format : public html_output_format
  {
  public:
    html_buffered_output_format (diagnostic_context &context,
				 const line_maps *line_maps,
				 const html_generation_options &html_gen_opts)
    : html_output_format (context, line_maps, html_gen_opts)
    {
    }
    bool machine_readable_stderr_p () const final override
    {
      return true;
    }
  };

  html_output_format *m_format; // borrowed
};

/* Test of reporting a diagnostic at UNKNOWN_LOCATION to a
   diagnostic_context and examining the generated XML document.
   Verify various basic properties. */

static void
test_simple_log ()
{
  test_html_diagnostic_context dc;

  rich_location richloc (line_table, UNKNOWN_LOCATION);
  dc.report (DK_ERROR, richloc, nullptr, 0, "this is a test: %qs", "foo");

  const xml::document &doc  = dc.get_document ();

  pretty_printer pp;
  doc.write_as_xml (&pp, 0, true);
  ASSERT_STREQ
    (pp_formatted_text (&pp),
     ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      "<!DOCTYPE html\n"
      "     PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
      "     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
      "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
      "  <head>\n"
      "    <title>Title goes here</title>\n"
      "  </head>\n"
      "  <body>\n"
      "    <div class=\"gcc-diagnostic-list\">\n"
      "      <div class=\"gcc-diagnostic\" id=\"gcc-diag-0\">\n"
      "        <span class=\"gcc-message\" id=\"gcc-diag-0-message\">this is a test: `<span class=\"gcc-quoted-text\">foo</span>&apos;</span>\n"
      "      </div>\n"
      "    </div>\n"
      "  </body>\n"
      "</html>\n"));
}

static void
test_metadata ()
{
  test_html_diagnostic_context dc;
  html_builder &b = dc.get_builder ();

  {
    diagnostic_metadata metadata;
    metadata.add_cwe (415);
    auto element = b.make_element_for_metadata (metadata);
    pretty_printer pp;
    element->write_as_xml (&pp, 0, true);
    ASSERT_STREQ
      (pp_formatted_text (&pp),
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
    diagnostic_metadata metadata;
    diagnostic_metadata::precanned_rule rule ("MISC-42",
					      "http://example.com");
    metadata.add_rule (rule);
    auto element = b.make_element_for_metadata (metadata);
    pretty_printer pp;
    element->write_as_xml (&pp, 0, true);
    ASSERT_STREQ
      (pp_formatted_text (&pp),
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

static void
test_printer ()
{
  xml::element top ("top", false);
  xml::printer xp (top);
  xp.push_tag ("foo");
  xp.add_text ("hello");
  xp.push_tag ("bar");
  xp.set_attr ("size", "3");
  xp.set_attr ("color", "red");
  xp.add_text ("world");
  xp.push_tag ("baz");
  xp.pop_tag ();
  xp.pop_tag ();
  xp.pop_tag ();

  pretty_printer pp;
  top.write_as_xml (&pp, 0, true);
  ASSERT_STREQ
    (pp_formatted_text (&pp),
     "<top>\n"
     "  <foo>\n"
     "    hello\n"
     "    <bar size=\"3\" color=\"red\">\n"
     "      world\n"
     "      <baz/>\n"
     "    </bar>\n"
     "  </foo>\n"
     "</top>\n");
}

// Verify that element attributes preserve insertion order.

static void
test_attribute_ordering ()
{
  xml::element top ("top", false);
  xml::printer xp (top);
  xp.push_tag ("chronological");
  xp.set_attr ("maldon", "991");
  xp.set_attr ("hastings", "1066");
  xp.set_attr ("edgehill", "1642");
  xp.set_attr ("naseby", "1645");
  xp.pop_tag ();
  xp.push_tag ("alphabetical");
  xp.set_attr ("edgehill", "1642");
  xp.set_attr ("hastings", "1066");
  xp.set_attr ("maldon", "991");
  xp.set_attr ("naseby", "1645");
  xp.pop_tag ();

  pretty_printer pp;
  top.write_as_xml (&pp, 0, true);
  ASSERT_STREQ
    (pp_formatted_text (&pp),
     "<top>\n"
     "  <chronological maldon=\"991\" hastings=\"1066\" edgehill=\"1642\" naseby=\"1645\"/>\n"
     "  <alphabetical edgehill=\"1642\" hastings=\"1066\" maldon=\"991\" naseby=\"1645\"/>\n"
     "</top>\n");
}

/* Run all of the selftests within this file.  */

void
diagnostic_format_html_cc_tests ()
{
  auto_fix_quotes fix_quotes;
  test_simple_log ();
  test_metadata ();
  test_printer ();
  test_attribute_ordering ();
}

} // namespace selftest

#endif /* CHECKING_P */
