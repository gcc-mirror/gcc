/* Helper code for graphviz output.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#define INCLUDE_MAP
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "graphviz.h"
#include "xml.h"
#include "xml-printer.h"
#include "pex.h"
#include "selftest.h"

dot::writer::writer (pretty_printer &pp)
: m_pp (pp),
  m_indent (0)
{
}

/* Print the current indent to the underlying pp.  */

void
dot::writer::write_indent ()
{
  for (int i = 0; i < m_indent * 4; ++i)
    pp_space (get_pp ());
}

graphviz_out::graphviz_out (pretty_printer *pp)
: writer (*pp)
{
  gcc_assert (pp);
}

/* Formatted print of FMT.  */

void
graphviz_out::print (const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  text_info text (fmt, &ap, errno);
  pp_format (get_pp (), &text);
  pp_output_formatted_text (get_pp ());
  va_end (ap);
}

/* Formatted print of FMT.  The text is indented by the current
   indent, and a newline is added.  */

void
graphviz_out::println (const char *fmt, ...)
{
  va_list ap;

  write_indent ();

  va_start (ap, fmt);
  text_info text (fmt, &ap, errno);
  pp_format (get_pp (), &text);
  pp_output_formatted_text (get_pp ());
  va_end (ap);

  pp_newline (get_pp ());
}

/* Write the start of an HTML-like row via <TR>, writing to the stream
   so that followup text can be escaped.  */

void
graphviz_out::begin_tr ()
{
  pp_string (get_pp (), "<TR>");
  pp_write_text_to_stream (get_pp ());
}

/* Write the end of an HTML-like row via </TR>, writing to the stream
   so that followup text can be escaped.  */

void
graphviz_out::end_tr ()
{
  pp_string (get_pp (), "</TR>");
  pp_write_text_to_stream (get_pp ());
}

/* Write the start of an HTML-like <TD>, writing to the stream
   so that followup text can be escaped.  */

void
graphviz_out::begin_td ()
{
  pp_string (get_pp (), "<TD ALIGN=\"LEFT\">");
  pp_write_text_to_stream (get_pp ());
}

/* Write the end of an HTML-like </TD>, writing to the stream
   so that followup text can be escaped.  */

void
graphviz_out::end_td ()
{
  pp_string (get_pp (), "</TD>");
  pp_write_text_to_stream (get_pp ());
}

/* Write the start of an HTML-like row via <TR><TD>, writing to the stream
   so that followup text can be escaped.  */

void
graphviz_out::begin_trtd ()
{
  pp_string (get_pp (), "<TR><TD ALIGN=\"LEFT\">");
  pp_write_text_to_stream (get_pp ());
}

/* Write the end of an HTML-like row via </TD></TR>, writing to the stream
   so that followup text can be escaped.  */

void
graphviz_out::end_tdtr ()
{
  pp_string (get_pp (), "</TD></TR>");
  pp_write_text_to_stream (get_pp ());
}

namespace dot {

// Definitions

// struct ast_node

void
ast_node::dump () const
{
  pretty_printer pp;
  pp.set_output_stream (stderr);
  writer w (pp);
  print (w);
  pp_newline (&pp);
  pp_flush (&pp);
}

// struct id

id::id (std::string str)
: m_str (std::move (str)),
  m_kind (is_identifier_p (m_str.c_str ())
	  ? kind::identifier
	  : kind::quoted)
{
}

id::id (const xml::node &n)
: m_kind (kind::html)
{
  pretty_printer pp;
  n.write_as_xml (&pp, 0, true);
  m_str = pp_formatted_text (&pp);
}

void
id::print (writer &w) const
{
  switch (m_kind)
    {
    default:
      gcc_unreachable ();

    case kind::identifier:
      w.write_string (m_str.c_str ());
      break;

    case kind::quoted:
      w.write_character ('"');
      for (auto ch : m_str)
	if (ch == '"')
	  w.write_string ("\\\"");
	else
	  w.write_character (ch);
      w.write_character ('"');
      break;

    case kind::html:
      w.write_character ('<');
      w.write_string (m_str.c_str ());
      w.write_character ('>');
      break;

    }
}

bool
id::is_identifier_p (const char *str)
{
  const char initial_ch = *str;
  if (initial_ch != '_' && !ISALPHA (initial_ch))
    return false;
  for (const char *iter = str + 1; *iter; ++iter)
    {
      const char iter_ch = *iter;
      if (iter_ch != '_' && !ISALNUM (iter_ch))
	return false;
    }
  return true;
}

// struct kv_pair

void
kv_pair::print (writer &w) const
{
  m_key.print (w);
  w.write_character ('=');
  m_value.print (w);
}

// struct attr_list

void
attr_list::print (writer &w) const
{
  if (m_kvs.empty ())
    return;
  w.write_string (" [");
  for (auto iter = m_kvs.begin (); iter != m_kvs.end (); ++iter)
    {
      if (iter != m_kvs.begin ())
	w.write_string ("; ");
      iter->print (w);
    }
  w.write_string ("]");
}

// struct stmt_list

void
stmt_list::print (writer &w) const
{
  for (auto &stmt : m_stmts)
    {
      w.write_indent ();
      stmt->print (w);
      w.write_string (";");
      w.write_newline ();
    }
}

void
stmt_list::add_edge (node_id src_id, node_id dst_id)
{
  m_stmts.push_back
    (std::make_unique<dot::edge_stmt>
     (std::move (src_id), std::move (dst_id)));
}

void
stmt_list::add_attr (id key, id value)
{
  add_stmt
    (std::make_unique <kv_stmt> (kv_pair (std::move (key),
					  std::move (value))));
}

// struct graph

void
graph::print (writer &w) const
{
  w.write_indent ();
  w.write_string ("digraph ");
  if (m_id)
    {
      m_id->print (w);
      w.write_character (' ');
    }
  w.write_string ("{");
  w.write_newline ();

  w.indent ();
  m_stmt_list.print (w);
  w.outdent ();

  w.write_indent ();
  w.write_string ("}");
  w.write_newline ();
}

// struct stmt_with_attr_list : public stmt

void
stmt_with_attr_list::set_label (dot::id value)
{
  m_attrs.add (dot::id ("label"), std::move (value));
}

// struct node_stmt : public stmt_with_attr_list

void
node_stmt::print (writer &w) const
{
  m_id.print (w);
  m_attrs.print (w);
}

// struct attr_stmt : public stmt_with_attr_list

void
attr_stmt::print (writer &w) const
{
  switch (m_kind)
    {
    default:
      gcc_unreachable ();
    case kind::graph:
      w.write_string ("graph");
      break;
    case kind::node:
      w.write_string ("node");
      break;
    case kind::edge:
      w.write_string ("edge");
      break;
    }
  m_attrs.print (w);
}

// struct kv_stmt : public stmt

void
kv_stmt::print (writer &w) const
{
  m_kv.print (w);
}

// struct node_id

void
node_id::print (writer &w) const
{
  m_id.print (w);
  if (m_port)
    m_port->print (w);
}

// struct port

void
port::print (writer &w) const
{
  if (m_id)
    {
      w.write_character (':');
      m_id->print (w);
    }
  if (m_compass_pt)
    {
      w.write_character (':');
      switch (*m_compass_pt)
	{
	default:
	  gcc_unreachable ();
	case compass_pt::n:
	  w.write_string ("n");
	  break;
	case compass_pt::ne:
	  w.write_string ("ne");
	  break;
	case compass_pt::e:
	  w.write_string ("e");
	  break;
	case compass_pt::se:
	  w.write_string ("se");
	  break;
	case compass_pt::s:
	  w.write_string ("s");
	  break;
	case compass_pt::sw:
	  w.write_string ("sw");
	  break;
	case compass_pt::w:
	  w.write_string ("w");
	  break;
	case compass_pt::nw:
	  w.write_string ("nw");
	  break;
	case compass_pt::c:
	  w.write_string ("c");
	  break;
	}
    }
}

// struct edge_stmt : public stmt_with_attr_list

void
edge_stmt::print (writer &w) const
{
  for (auto iter = m_node_ids.begin (); iter != m_node_ids.end (); ++iter)
    {
      if (iter != m_node_ids.begin ())
	w.write_string (" -> ");
      iter->print (w);
    }
  m_attrs.print (w);
}

// struct subgraph : public stmt

void
subgraph::print (writer &w) const
{
  w.write_newline ();
  w.write_indent ();
  w.write_string ("subgraph ");
  m_id.print (w);
  w.write_string (" {");
  w.write_newline ();

  w.indent ();
  m_stmt_list.print (w);
  w.outdent ();
  w.write_newline ();

  w.write_indent ();
  w.write_string ("}"); // newline and semicolon added by stmt_list
}

/* Convert G to graphviz source, attempt to invoke "dot -Tsvg" on it
   as a subprocess, and get the SVG source from stdout, or nullptr
   if there was a problem.  */

static std::unique_ptr<std::string>
make_svg_document_buffer_from_graph (const graph &g)
{
  /* Ideally there would be a way of doing this without
     invoking dot as a subprocess.  */

  std::vector<std::string> args;
  args.push_back ("dot");
  args.push_back ("-Tsvg");

  pex p (0, "dot", nullptr);

  {
    auto pipe_stdin = p.input_file (true, nullptr);
    gcc_assert (pipe_stdin.m_file);
    pretty_printer pp;
    pp.set_output_stream (pipe_stdin.m_file);
    writer w (pp);
    g.print (w);
    pp_flush (&pp);
  }

  int err = 0;
  const char * errmsg
    = p.run (PEX_SEARCH,
	     "dot", args, nullptr, nullptr, &err);
  auto pipe_stdout = p.read_output ();
  auto content = pipe_stdout.read_all ();

  if (errmsg)
    return nullptr;
  if (err)
    return nullptr;

  std::string result;
  result.reserve (content->size () + 1);
  for (auto &iter : *content)
    result.push_back (iter);
  return std::make_unique<std::string> (std::move (result));
}

/* Convert G to graphviz source, attempt to invoke "dot -Tsvg" on it
   as a subprocess, and get the SVG source from stdout, and extract
   the "svg" subtree as an xml::raw node.

   Note that this
   (a) invokes "dot" as a subprocess
   (b) assumes that we trust the output from "dot".

   Return nullptr if there was a problem.  */

std::unique_ptr<xml::node>
make_svg_from_graph (const graph &g)
{
  auto svg_src = make_svg_document_buffer_from_graph (g);
  if (!svg_src)
    return nullptr;

  /* Skip past the XML header to the parts we care about.  */
  auto pos = svg_src->find ("<!-- Generated by graphviz");
  if (pos == svg_src->npos)
    return nullptr;

  auto substring = std::string (*svg_src, pos);
  return std::make_unique<xml::raw> (std::move (substring));
}

} // namespace dot

#if CHECKING_P

namespace selftest {

static void
test_ids ()
{
  ASSERT_TRUE (dot::id::is_identifier_p ("foo"));
  ASSERT_FALSE (dot::id::is_identifier_p ("hello world"));
  ASSERT_TRUE (dot::id::is_identifier_p ("foo42"));
  ASSERT_FALSE (dot::id::is_identifier_p ("42"));
  ASSERT_TRUE (dot::id::is_identifier_p ("_"));
}

static void
test_trivial_graph ()
{
  dot::graph g;
  // node "a"
  {
    g.add_stmt (std::make_unique<dot::node_stmt> (dot::id ("a")));
  }
  // node "b"
  {
    auto n = std::make_unique<dot::node_stmt> (dot::id ("b"));
    n->m_attrs.add (dot::id ("label"), dot::id ("This is node b"));
    n->m_attrs.add (dot::id ("color"), dot::id ("green"));
    g.add_stmt (std::move (n));
  }
  // an edge between them
  {
    auto e = std::make_unique<dot::edge_stmt> (dot::id ("a"),
					       dot::id ("b"));
    e->m_attrs.add (dot::id ("label"), dot::id ("I'm an edge"));
    g.add_stmt (std::move (e));
  }
  pretty_printer pp;
  dot::writer w (pp);
  g.print (w);
  ASSERT_STREQ
    (pp_formatted_text (&pp),
     ("digraph {\n"
      "    a;\n"
      "    b [label=\"This is node b\"; color=green];\n"
      "    a -> b [label=\"I'm an edge\"];\n"
      "}\n"));
}

/* Recreating the HTML record example from
   https://graphviz.org/doc/info/shapes.html#html  */

static void
test_layout_example ()
{
  dot::graph g (dot::id ("structs"));

  // "node [shape=plaintext]\n"
  {
    auto attr_stmt
      = std::make_unique<dot::attr_stmt> (dot::attr_stmt::kind::node);
    attr_stmt->m_attrs.add (dot::id ("shape"), dot::id ("plaintext"));
    g.add_stmt (std::move (attr_stmt));
  }

  // struct1
  {
    auto n = std::make_unique<dot::node_stmt> (dot::id ("struct1"));

    xml::element table ("TABLE", false);
    xml::printer xp (table);
    xp.set_attr ("BORDER", "0");
    xp.set_attr ("CELLBORDER", "1");
    xp.set_attr ("CELLSPACING", "0");

    xp.push_tag ("TR", true);

    xp.push_tag ("TD", false);
    xp.add_text ("left");
    xp.pop_tag ("TD");

    xp.push_tag ("TD", false);
    xp.set_attr ("PORT", "f1");
    xp.add_text ("mid dle");
    xp.pop_tag ("TD");

    xp.push_tag ("TD", false);
    xp.set_attr ("PORT", "f2");
    xp.add_text ("right");
    xp.pop_tag ("TD");

    n->set_label (table);
    g.add_stmt (std::move (n));
  }

  // struct2
  {
    auto n = std::make_unique<dot::node_stmt> (dot::id ("struct2"));
    xml::element table ("TABLE", false);
    xml::printer xp (table);
    xp.set_attr ("BORDER", "0");
    xp.set_attr ("CELLBORDER", "1");
    xp.set_attr ("CELLSPACING", "0");

    xp.push_tag ("TR", true);

    xp.push_tag ("TD", false);
    xp.set_attr ("PORT", "f0");
    xp.add_text ("one");
    xp.pop_tag ("TD");

    xp.push_tag ("TD", false);
    xp.add_text ("two");
    xp.pop_tag ("TD");

    n->set_label (table);
    g.add_stmt (std::move (n));
  }

  // struct3
  {
    auto n = std::make_unique<dot::node_stmt> (dot::id ("struct3"));
    xml::element table ("TABLE", false);
    xml::printer xp (table);
    xp.set_attr ("BORDER", "0");
    xp.set_attr ("CELLBORDER", "1");
    xp.set_attr ("CELLSPACING", "0");
    xp.set_attr ("CELLPADDING", "4");

    xp.push_tag ("TR", false);

    xp.push_tag ("TD", true);
    xp.set_attr ("ROWSPAN", "3");
    xp.add_text ("hello");
    xp.append (std::make_unique<xml::element> ("BR", false));
    xp.add_text ("world");
    xp.pop_tag ("TD");

    xp.push_tag ("TD", true);
    xp.set_attr ("COLSPAN", "3");
    xp.add_text ("b");
    xp.pop_tag ("TD");

    xp.push_tag ("TD", true);
    xp.set_attr ("ROWSPAN", "3");
    xp.add_text ("g");
    xp.pop_tag ("TD");

    xp.push_tag ("TD", true);
    xp.set_attr ("ROWSPAN", "3");
    xp.add_text ("h");
    xp.pop_tag ("TD");

    xp.pop_tag ("TR");

    xp.push_tag ("TR", false);

    xp.push_tag ("TD", true);
    xp.add_text ("c");
    xp.pop_tag ("TD");

    xp.push_tag ("TD", true);
    xp.set_attr ("PORT", "here");
    xp.add_text ("d");
    xp.pop_tag ("TD");

    xp.push_tag ("TD", true);
    xp.add_text ("e");
    xp.pop_tag ("TD");

    xp.pop_tag ("TR");

    xp.push_tag ("TR", false);

    xp.push_tag ("TD", true);
    xp.set_attr ("COLSPAN", "3");
    xp.add_text ("f");
    xp.pop_tag ("TD");

    n->set_label (table);
    g.add_stmt (std::move (n));
  }

  g.m_stmt_list.add_edge
    (dot::node_id (dot::id ("struct1"),
		   dot::port (dot::id ("f1"))),
     dot::node_id (dot::id ("struct2"),
		   dot::port (dot::id ("f0"))));
  g.m_stmt_list.add_edge
    (dot::node_id (dot::id ("struct1"),
		   dot::port (dot::id ("f2"))),
     dot::node_id (dot::id ("struct3"),
		   dot::port (dot::id ("here"))));

  pretty_printer pp;
  dot::writer w (pp);
  g.print (w);

  /* There are some whitespace differences with the example in the
     GraphViz docs.  */
  ASSERT_STREQ
    (pp_formatted_text (&pp),
     ("digraph structs {\n"
      "    node [shape=plaintext];\n" // added semicolon
      "    struct1 [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">\n"
      "  <TR><TD>left</TD><TD PORT=\"f1\">mid dle</TD><TD PORT=\"f2\">right</TD></TR>\n"
      "</TABLE>\n"
      ">];\n"
      "    struct2 [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">\n"
      "  <TR><TD PORT=\"f0\">one</TD><TD>two</TD></TR>\n"
      "</TABLE>\n"
      ">];\n"
      "    struct3 [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\" CELLPADDING=\"4\">\n"
      "  <TR>\n"
      "    <TD ROWSPAN=\"3\">hello<BR/>world</TD>\n"
      "    <TD COLSPAN=\"3\">b</TD>\n"
      "    <TD ROWSPAN=\"3\">g</TD>\n"
      "    <TD ROWSPAN=\"3\">h</TD>\n"
      "  </TR>\n"
      "  <TR>\n"
      "    <TD>c</TD>\n"
      "    <TD PORT=\"here\">d</TD>\n"
      "    <TD>e</TD>\n"
      "  </TR>\n"
      "  <TR>\n"
      "    <TD COLSPAN=\"3\">f</TD>\n"
      "  </TR>\n"
      "</TABLE>\n"
      ">];\n"
      "    struct1:f1 -> struct2:f0;\n"
      "    struct1:f2 -> struct3:here;\n"
      "}\n"));
}

/* Run all of the selftests within this file.  */

void
graphviz_cc_tests ()
{
  test_ids ();
  test_trivial_graph ();
  test_layout_example ();
}

} // namespace selftest

#endif /* CHECKING_P */
