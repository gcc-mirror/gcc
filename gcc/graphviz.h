/* Helper code for graphviz output.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

#ifndef GCC_GRAPHVIZ_H
#define GCC_GRAPHVIZ_H

#include "pretty-print.h" /* for ATTRIBUTE_GCC_PPDIAG.  */

namespace xml { class node; }

namespace dot {

/* A class for writing .dot output to a pretty_printer with
   indentation to show nesting.  */

class writer {
public:
  writer (pretty_printer &pp);

  void indent () { m_indent++; }
  void outdent () { m_indent--; }

  void write_indent ();

  void write_character (char ch)
  {
    pp_character (&m_pp, ch);
  }
  void write_string (const char *str)
  {
    pp_string (&m_pp, str);
  }
  void write_newline ()
  {
    pp_newline (&m_pp);
  }

  pretty_printer *get_pp () const { return &m_pp; }

 private:
  pretty_printer &m_pp;
  int m_indent;
};

// An AST for the dot language
// See https://graphviz.org/doc/info/lang.html

// Forward decls

struct id;
struct node_id;
struct port;
struct kv_pair;
struct graph;
struct attr_list;
struct stmt_list;
struct stmt;
  struct node_stmt;
  struct attr_stmt;
  struct kv_stmt;
  struct edge_stmt;
  struct subgraph;

// Decls

struct ast_node
{
  virtual ~ast_node () {}
  virtual void print (writer &w) const = 0;

  void dump () const;
};

struct id : public ast_node
{
  enum class kind
  {
    identifier,
    quoted,
    html
  };

  id (std::string str);

  /* For HTML labels: see https://graphviz.org/doc/info/shapes.html#html  */
  id (const xml::node &n);

  void print (writer &w) const final override;

  static bool is_identifier_p (const char *);

  std::string m_str;
  enum kind m_kind;
};

/* ID '=' ID */

struct kv_pair : ast_node
{
  kv_pair (id key, id value)
  : m_key (std::move (key)),
    m_value (std::move (value))
  {
  }

  void print (writer &w) const final override;

  id m_key;
  id m_value;
};

/* attr_list: '[' [ a_list ] ']' [ attr_list ] */

struct attr_list : public ast_node
{
  void print (writer &w) const;
  void add (id key, id value)
  {
    m_kvs.push_back ({std::move (key), std::move (value)});
  }

  std::vector<kv_pair> m_kvs;
};

/* stmt_list : [ stmt [ ';' ] stmt_list ] */

struct stmt_list : public ast_node
{
  void print (writer &w) const final override;
  void add_stmt (std::unique_ptr<stmt> s)
  {
    m_stmts.push_back (std::move (s));
  }
  void add_edge (node_id src_id, node_id dst_id);
  void add_attr (id key, id value);

  std::vector<std::unique_ptr<stmt>> m_stmts;
};

/* graph : [ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'  */

struct graph : public ast_node
{
  graph ()
  : m_id (nullptr)
  {
  }

  graph (id id_)
  : m_id (std::make_unique<id> (std::move (id_)))
  {
  }

  void print (writer &w) const final override;

  void add_stmt (std::unique_ptr<stmt> s)
  {
    m_stmt_list.add_stmt (std::move (s));
  }

  std::unique_ptr<id> m_id; // optional
  stmt_list m_stmt_list;
};

/* Abstract base class.
     stmt : node_stmt
	  | edge_stmt
	  | attr_stmt
	  | ID '=' ID ("kv_stmt")
	  | subgraph  */

struct stmt
{
  virtual ~stmt () {}
  virtual void print (writer &w) const = 0;
};

struct stmt_with_attr_list : public stmt
{
  void set_attr (id key, id value)
  {
    m_attrs.add (std::move (key), std::move (value));
  }
  void set_label (dot::id label);

  attr_list m_attrs;
};

struct node_stmt : public stmt_with_attr_list
{
  node_stmt (id id_)
  : m_id (id_)
  {
  }

  void print (writer &w) const final override;

  id m_id;
};

struct attr_stmt : public stmt_with_attr_list
{
  enum class kind { graph, node, edge };

  attr_stmt (enum kind kind_)
  : m_kind (kind_)
  {
  }

  void print (writer &w) const final override;

  enum kind m_kind;
};

/* "ID '=' ID" as a stmt.  */

struct kv_stmt : public stmt
{
  kv_stmt (kv_pair kv)
  : m_kv (std::move (kv))
  {}

  void print (writer &w) const final override;

  kv_pair m_kv;
};

/* node_id : ID [ port ] */

enum class compass_pt
{
 n, ne, e, se, s, sw, w, nw, c
 /* "_" clashes with intl macro */
};

bool
get_compass_pt_from_string (const char *str, enum compass_pt &out);

/* port : ':' ID [ ':' compass_pt ]
        | ':' compass_pt
*/

struct port : public ast_node
{
  port (id id_)
  : m_id (std::make_unique<id> (std::move (id_))),
    m_compass_pt (nullptr)
  {
  }

  port (enum compass_pt compass_pt_)
  : m_id (nullptr),
    m_compass_pt (std::make_unique<compass_pt> (compass_pt_))
  {
  }

  port (id id_,
	enum compass_pt compass_pt_)
  : m_id (std::make_unique<id> (std::move (id_))),
    m_compass_pt (std::make_unique<compass_pt> (compass_pt_))
  {
  }

  port (const port &other)
  : m_id (nullptr),
    m_compass_pt (nullptr)
  {
    if (other.m_id)
      m_id = std::make_unique<id> (*other.m_id);
    if (other.m_compass_pt)
      m_compass_pt = std::make_unique<enum compass_pt> (*other.m_compass_pt);
  }

  void print (writer &w) const final override;

  std::unique_ptr<id> m_id; // would be std::optional
  std::unique_ptr<enum compass_pt> m_compass_pt; // would be std::optional
};

struct node_id : public ast_node
{
  node_id (id id_)
  : m_id (id_),
    m_port (nullptr)
  {
  }
  node_id (id id_, port port_)
  : m_id (id_),
    m_port (std::make_unique<port> (std::move (port_)))
  {
  }
  node_id (const node_id &other)
  : m_id (other.m_id),
    m_port (nullptr)
  {
    if (other.m_port)
      m_port = std::make_unique<port> (*other.m_port);
  }

  node_id &operator= (const node_id &other)
  {
    m_id = other.m_id;
    if (other.m_port)
      m_port = std::make_unique<port> (*other.m_port);
    else
      m_port = nullptr;
    return *this;
  }

  void print (writer &w) const final override;

  id m_id;
  std::unique_ptr<port> m_port; // would be std::optional
};

/* The full grammar for edge_stmt is:
     edge_stmt  : (node_id | subgraph) edgeRHS [ attr_list ]
     edgeRHS    : edgeop (node_id | subgraph) [ edgeRHS ]
   This class support the subsets where all are "node_id", rather than
   "subgraph", and doesn't yet support "port" giving effectively:
      node_id (edgeop node_id)+ [ attr_list]
 */

struct edge_stmt : public stmt_with_attr_list
{
  edge_stmt (node_id src_id, node_id dst_id)
  {
    m_node_ids.push_back (std::move (src_id));
    m_node_ids.push_back (std::move (dst_id));
  }

  void print (writer &w) const final override;

  std::vector<node_id> m_node_ids; // should have 2 or more elements
};

/* [ subgraph [ ID ] ] '{' stmt_list '}' */

struct subgraph : public stmt
{
  subgraph (id id_)
  : m_id (id_)
  {
  }

  void print (writer &w) const final override;

  void add_stmt (std::unique_ptr<stmt> s)
  {
    m_stmt_list.add_stmt (std::move (s));
  }
  void add_attr (id key, id value)
  {
    m_stmt_list.add_stmt
      (std::make_unique <kv_stmt> (kv_pair (std::move (key),
					    std::move (value))));
  }

  id m_id;
  stmt_list m_stmt_list;
};

extern std::unique_ptr<xml::node>
make_svg_from_graph (const graph &g);

} // namespace dot

/* A class for writing .dot output to a pretty_printer with
   indentation to show nesting.  */

class graphviz_out : public dot::writer {
 public:
  graphviz_out (pretty_printer *pp);

  void print (const char *fmt, ...)
    ATTRIBUTE_GCC_PPDIAG(2,3);
  void println (const char *fmt, ...)
    ATTRIBUTE_GCC_PPDIAG(2,3);

  void begin_tr ();
  void end_tr ();

  void begin_td ();
  void end_td ();

  void begin_trtd ();
  void end_tdtr ();
};

#endif /* GCC_GRAPHVIZ_H */
