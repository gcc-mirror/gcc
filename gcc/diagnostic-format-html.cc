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
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "diagnostic.h"
#include "diagnostic-metadata.h"
#include "diagnostic-format.h"
#include "diagnostic-format-html.h"
#include "diagnostic-output-file.h"
#include "diagnostic-buffer.h"
#include "selftest.h"
#include "selftest-diagnostic.h"
#include "pretty-print-format-impl.h"
#include "intl.h"

namespace xml {

/* Disable warnings about quoting issues in the pp_xxx calls below
   that (intentionally) don't follow GCC diagnostic conventions.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif

struct node
{
  virtual ~node () {}
  virtual void write_as_xml (pretty_printer *pp,
			     int depth, bool indent) const = 0;
  void dump (FILE *out) const;
  void DEBUG_FUNCTION dump () const { dump (stderr); }
};

struct text : public node
{
  text (label_text str)
  : m_str (std::move (str))
  {}

  void write_as_xml (pretty_printer *pp,
		     int depth, bool indent) const final override;

  label_text m_str;
};

struct node_with_children : public node
{
  void add_child (std::unique_ptr<node> node);
  void add_text (label_text str);

  std::vector<std::unique_ptr<node>> m_children;
};

struct document : public node_with_children
{
  void write_as_xml (pretty_printer *pp,
		     int depth, bool indent) const final override;
};

struct element : public node_with_children
{
  element (const char *kind, bool preserve_whitespace)
  : m_kind (kind),
    m_preserve_whitespace (preserve_whitespace)
  {}

  void write_as_xml (pretty_printer *pp,
		     int depth, bool indent) const final override;

  void set_attr (const char *name, label_text value);

  const char *m_kind;
  bool m_preserve_whitespace;
  std::map<const char *, label_text> m_attributes;
};

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
text::write_as_xml (pretty_printer *pp, int /*depth*/, bool /*indent*/) const
{
  write_escaped_text (pp, m_str.get ());
}

/* struct node_with_children : public node.  */

void
node_with_children::add_child (std::unique_ptr<node> node)
{
  gcc_assert (node.get ());
  m_children.push_back (std::move (node));
}

void
node_with_children::add_text (label_text str)
{
  gcc_assert (str.get ());
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
  for (auto &iter : m_children)
    iter->write_as_xml (pp, depth, indent);
}

/* struct element : public node_with_children.  */

void
element::write_as_xml (pretty_printer *pp, int depth, bool indent) const
{
  if (indent)
    {
      pp_newline (pp);
      for (int i = 0; i < depth; ++i)
	pp_string (pp, "  ");
    }

  if (m_preserve_whitespace)
    indent = false;

  pp_printf (pp, "<%s", m_kind);
  for (auto &attr : m_attributes)
    {
      pp_printf (pp, " %s=\"", attr.first);
      write_escaped_text (pp, attr.second.get ());
      pp_string (pp, "\"");
    }
  if (m_children.empty ())
    pp_string (pp, " />");
  else
    {
      pp_string (pp, ">");
      for (auto &child : m_children)
	child->write_as_xml (pp, depth + 1, indent);
      if (indent)
	{
	  pp_newline (pp);
	  for (int i = 0; i < depth; ++i)
	    pp_string (pp, "  ");
	}
      pp_printf (pp, "</%s>", m_kind);
    }
}

void
element::set_attr (const char *name, label_text value)
{
  m_attributes[name] = std::move (value);
}

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif

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
		 const line_maps *line_maps);

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

private:
  std::unique_ptr<xml::element>
  make_element_for_diagnostic (const diagnostic_info &diagnostic,
			       diagnostic_t orig_diag_kind);

  diagnostic_context &m_context;
  pretty_printer *m_printer;
  const line_maps *m_line_maps;

  std::unique_ptr<xml::document> m_document;
  xml::element *m_diagnostics_element;
  std::unique_ptr<xml::element> m_cur_diagnostic_element;
};

static std::unique_ptr<xml::element>
make_div (label_text class_)
{
  auto div = std::make_unique<xml::element> ("div", false);
  div->set_attr ("class", std::move (class_));
  return div;
}

static std::unique_ptr<xml::element>
make_span (label_text class_)
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

/* html_builder's ctor.  */

html_builder::html_builder (diagnostic_context &context,
			      pretty_printer &pp,
			      const line_maps *line_maps)
: m_context (context),
  m_printer (&pp),
  m_line_maps (line_maps)
{
  gcc_assert (m_line_maps);

  m_document = std::make_unique<xml::document> ();
  {
    auto html_element = std::make_unique<xml::element> ("html", false);
    html_element->set_attr
      ("xmlns",
       label_text::borrow ("http://www.w3.org/1999/xhtml"));
    {
      {
	auto head_element = std::make_unique<xml::element> ("head", false);
	{
	  auto title_element = std::make_unique<xml::element> ("title", true);
	  label_text title (label_text::borrow ("Title goes here")); // TODO
	  title_element->add_text (std::move (title));
	  head_element->add_child (std::move (title_element));
	}
	html_element->add_child (std::move (head_element));

	auto body_element = std::make_unique<xml::element> ("body", false);
	{
	  auto diagnostics_element
	    = make_div (label_text::borrow ("gcc-diagnostic-list"));
	  m_diagnostics_element = diagnostics_element.get ();
	  body_element->add_child (std::move (diagnostics_element));
	}
	html_element->add_child (std::move (body_element));
      }
    }
    m_document->add_child (std::move (html_element));
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

std::unique_ptr<xml::element>
html_builder::make_element_for_diagnostic (const diagnostic_info &diagnostic,
					   diagnostic_t orig_diag_kind)
{
  class html_token_printer : public token_printer
  {
  public:
    html_token_printer (html_builder &builder,
			 xml::element &parent_element)
    : m_builder (builder)
    {
      m_open_elements.push_back (&parent_element);
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
	      insertion_element ().add_text
		(label_text::take (xstrdup (sub->m_value.get ())));
	    }
	    break;

	  case pp_token::kind::begin_color:
	  case pp_token::kind::end_color:
	    /* These are no-ops.  */
	    break;

	  case pp_token::kind::begin_quote:
	    {
	      insertion_element ().add_text (label_text::borrow (open_quote));
	      push_element (make_span (label_text::borrow ("gcc-quoted-text")));
	    }
	    break;
	  case pp_token::kind::end_quote:
	    {
	      pop_element ();
	      insertion_element ().add_text (label_text::borrow (close_quote));
	    }
	    break;

	  case pp_token::kind::begin_url:
	    {
	      pp_token_begin_url *sub = as_a <pp_token_begin_url *> (iter);
	      auto anchor = std::make_unique<xml::element> ("a", true);
	      anchor->set_attr ("href", std::move (sub->m_value));
	      push_element (std::move (anchor));
	    }
	    break;
	  case pp_token::kind::end_url:
	    pop_element ();
	    break;
	  }
    }

  private:
    xml::element &insertion_element () const
    {
      return *m_open_elements.back ();
    }
    void push_element (std::unique_ptr<xml::element> new_element)
    {
      xml::element &current_top = insertion_element ();
      m_open_elements.push_back (new_element.get ());
      current_top.add_child (std::move (new_element));
    }
    void pop_element ()
    {
      m_open_elements.pop_back ();
    }

    html_builder &m_builder;
    /* We maintain a stack of currently "open" elements.
       Children are added to the topmost open element.  */
    std::vector<xml::element *> m_open_elements;
  };

  auto diag_element = make_div (label_text::borrow ("gcc-diagnostic"));

  // TODO: might be nice to emulate the text output format, but colorize it

  auto message_span = make_span (label_text::borrow ("gcc-message"));
  html_token_printer tok_printer (*this, *message_span.get ());
  m_printer->set_token_printer (&tok_printer);
  pp_output_formatted_text (m_printer, m_context.get_urlifier ());
  m_printer->set_token_printer (nullptr);
  pp_clear_output_area (m_printer);
  diag_element->add_child (std::move (message_span));

  if (diagnostic.metadata)
    {
      int cwe = diagnostic.metadata->get_cwe ();
      if (cwe)
	{
	  diag_element->add_text (label_text::borrow (" "));
	  auto cwe_span = make_span (label_text::borrow ("gcc-cwe-metadata"));
	  cwe_span->add_text (label_text::borrow ("["));
	  {
	    auto anchor = std::make_unique<xml::element> ("a", true);
	    anchor->set_attr ("href", label_text::take (get_cwe_url (cwe)));
	    pretty_printer pp;
	    pp_printf (&pp, "CWE-%i", cwe);
	    anchor->add_text
	      (label_text::take (xstrdup (pp_formatted_text (&pp))));
	    cwe_span->add_child (std::move (anchor));
	  }
	  cwe_span->add_text (label_text::borrow ("]"));
	  diag_element->add_child (std::move (cwe_span));
	}
    }

  // TODO: show any rules

  label_text option_text = label_text::take
    (m_context.make_option_name (diagnostic.option_id,
				 orig_diag_kind, diagnostic.kind));
  if (option_text.get ())
    {
      label_text option_url = label_text::take
	(m_context.make_option_url (diagnostic.option_id));

      diag_element->add_text (label_text::borrow (" "));
      auto option_span = make_span (label_text::borrow ("gcc-option"));
      option_span->add_text (label_text::borrow ("["));
      {
	if (option_url.get ())
	  {
	    auto anchor = std::make_unique<xml::element> ("a", true);
	    anchor->set_attr ("href", std::move (option_url));
	    anchor->add_text (std::move (option_text));
	    option_span->add_child (std::move (anchor));
	  }
	else
	  option_span->add_text (std::move (option_text));
	option_span->add_text (label_text::borrow ("]"));
      }
      diag_element->add_child (std::move (option_span));
    }

  {
    auto pre = std::make_unique<xml::element> ("pre", true);
    pre->set_attr ("class", label_text::borrow ("gcc-annotated-source"));
    // TODO: ideally we'd like to capture elements within the following:
    diagnostic_show_locus (&m_context, m_context.m_source_printing,
			   diagnostic.richloc, diagnostic.kind,
			   m_printer);
    pre->add_text
      (label_text::take (xstrdup (pp_formatted_text (m_printer))));
    pp_clear_output_area (m_printer);
    diag_element->add_child (std::move (pre));
  }

  return diag_element;
}

/* Implementation of diagnostic_context::m_diagrams.m_emission_cb
   for HTML output.  */

void
html_builder::emit_diagram (const diagnostic_diagram &/*diagram*/)
{
  /* We must be within the emission of a top-level diagnostic.  */
  gcc_assert (m_cur_diagnostic_element);

  // TODO
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

protected:
  html_output_format (diagnostic_context &context,
		      const line_maps *line_maps)
  : diagnostic_output_format (context),
    m_builder (context, *get_printer (), line_maps),
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
			   diagnostic_output_file output_file)
  : html_output_format (context, line_maps),
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
		diagnostic_output_file output_file)
{
  auto sink
    = std::make_unique<html_file_output_format> (context,
						 &line_maps,
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
    auto sink = std::make_unique<html_buffered_output_format> (*this,
							       line_table);
    sink->update_printer ();
    m_format = sink.get (); // borrowed

    set_output_format (std::move (sink));
  }

  const xml::document &get_document () const
  {
    return m_format->get_document ();
  }

private:
  class html_buffered_output_format : public html_output_format
  {
  public:
    html_buffered_output_format (diagnostic_context &context,
				 const line_maps *line_maps)
    : html_output_format (context, line_maps)
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
  dc.report (DK_ERROR, richloc, nullptr, 0, "this is a test: %i", 42);

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
      "      <div class=\"gcc-diagnostic\">\n"
      "        <span class=\"gcc-message\">this is a test: 42</span>\n"
      "        <pre class=\"gcc-annotated-source\"></pre>\n"
      "      </div>\n"
      "    </div>\n"
      "  </body>\n"
      "</html>"));
}

/* Run all of the selftests within this file.  */

void
diagnostic_format_html_cc_tests ()
{
  test_simple_log ();
}

} // namespace selftest

#endif /* CHECKING_P */
