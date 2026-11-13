/* Base class for svalues and regions.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_SYMBOLIC_NODES_H
#define GCC_ANALYZER_SYMBOLIC_NODES_H

#include "pretty-print-markup.h"

#include "analyzer/analyzer-logging.h"
#include "analyzer/complexity.h"
#include "analyzer/units.h"
#include "analyzer/integer-with-unit.h"

#include "text-art/tree-widget.h"
#include "text-art/dump.h"

namespace ana {
namespace symbolic {

/* A central hub for managing symbolic::node instances
   - Provides a central authority for handing out IDs for new nodes.
   - Owns the various managers of symbolic::node subclasses and provides
   a central place for them to find each other.

   We use pointers to the managers to minimize compile-time dependencies.

   This is effectively a singleton during analysis (but each unit test
   gets its own instance).  */

class node_manager
{
public:
  node_manager ();
  ~node_manager ();

  node_manager (const node_manager &) = delete;

  unsigned get_num_nodes () const { return m_next_node_id; }
  unsigned alloc_node_id () { return m_next_node_id++; }

  region_model_manager &
  get_region_model_manager () const { return *m_rmm; }

  region_model_manager &
  get_svalue_manager () const { return *m_rmm; }

  symbolic::bit_contents_manager &
  get_bit_contents_manager () const { return *m_bit_contents_manager; }

  symbolic::bit_vector_manager &
  get_bit_vector_manager () const { return *m_bit_vector_manager; }

  symbolic::scalar_managers &
  get_scalars () const { return *m_scalar_managers; }

  void log_stats (logger *logger, bool show_objs) const;

private:
  unsigned m_next_node_id;

  std::unique_ptr<symbolic::scalar_managers> m_scalar_managers;
  std::unique_ptr<region_model_manager> m_rmm;
  std::unique_ptr<symbolic::bit_contents_manager> m_bit_contents_manager;
  std::unique_ptr<symbolic::bit_vector_manager> m_bit_vector_manager;
};

/* A bundle of information for use by symbolic::node's
   make_markup_node vfunc.  */

struct markup_context
{
  markup_context (const region_model *model,
		  bool user_facing)
  : m_model (model),
    m_user_facing (user_facing)
  {
  }

  const region_model *m_model;
  bool m_user_facing;
};

/* Abstract base class for the various tree-like purely-functional
   data structures we use during symbolic execution.

   Instances are owned by a manager and reused, with memoization/caching
   (compare to "hash consing" and value numbering).

   Instances have a complexity and a numeric ID.

   Has hierarchical dumping for debugging how nodes are built from other
   nodes.

   Has a node_visitor class for walking the object hierarchy.

   Can convert itself to json for debugging/SARIF.  */

class node
{
 public:
  using id_t = unsigned;

  virtual ~node () = default;

  /* Instances are managed via one of the managers within node_manager
     and thus shouldn't be copied/assigned.  */
  node (const node &) = delete;
  node (node &&) = delete;
  node &operator= (const node &) = delete;
  node &operator= (node &&) = delete;

  const complexity &get_complexity () const { return m_complexity; }

  id_t get_id () const { return m_id; }
  static int cmp_ids (const symbolic::node *s1, const symbolic::node *s2);
  static int cmp (const symbolic::node &s1, const symbolic::node &s2)
  {
    return (int)s1.get_id () - (int)s2.get_id ();
  }
  static int cmp_ptr_ptr (const void *, const void *);

  void dump_to_pp (pretty_printer *pp,
		   bool show_ids = true,
		   bool elide_repeated_nodes = true) const;
  void dump_to_logger (logger &) const;
  void dump () const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (node_dump_state &nds,
		    const char *prefix = nullptr) const;

  virtual const char *
  get_any_dump_color () const { return nullptr; }

  virtual void
  accept_node_visitor (node_visitor &v) const = 0;

  virtual std::unique_ptr<json::object>
  to_json () const = 0;

  virtual std::unique_ptr<diagnostics::markup::node>
  make_markup_node (markup_context &markup_ctxt) const = 0;

  bool involves_p (const symbolic::node &other) const;

  void
  start_any_dump_color (pretty_printer *pp) const;

  void
  end_any_dump_color (pretty_printer *pp) const;

  text_art::style
  get_text_art_tree_style () const;

 protected:
  node (complexity c, unsigned id)
  : m_complexity (c),
    m_id (id)
  {}

  std::unique_ptr<json::object>
  make_json_obj (const char *type_name) const;

 private:
  virtual void
  print_dump_widget_label (pretty_printer *pp) const = 0;
  virtual void
  add_dump_widget_children (text_art::tree_widget &,
			    node_dump_state &nds) const = 0;

  complexity m_complexity;
  id_t m_id; // for deterministic sorting at this stage, for dumps
};

struct CompareNodeIds
{
  bool operator()(const node *lhs, const node *rhs) const
  {
    return node::cmp_ids (lhs, rhs) < 0;
  }
};

/* A pp_element subclass for printing a symbolic::node in pp_printf via %e.
   This uses node::make_markup_node, lowers that, and prints the result
   to the pretty_printer.  */

class pp_element_for_node : public pp_element
{
public:
  pp_element_for_node (const node &n, bool user_facing = true)
  : m_node (n),
    m_user_facing (user_facing)
  {
  }

  void add_to_phase_2 (pp_markup::context &ctxt) final override;

private:
  const node &m_node;
  bool m_user_facing;
};

/* A set of symbolic::node instances.  */

template <typename NodeClass>
class node_set : public std::set<const NodeClass *, CompareNodeIds>
{
public:
  void add (const NodeClass *n)
  {
    gcc_assert (n);
    this->insert (n);
  }
  bool contains (const NodeClass *n) const;

  node_set<NodeClass>
  union_ (const node_set<NodeClass> &other) const;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (node_dump_state &nds,
		    const char *prefix = nullptr) const;

  void dump_to_pp (pretty_printer *pp,
		   bool show_ids = true,
		   bool elide_repeated_nodes = true) const;
  void dump () const;

  bool operator< (const node_set<NodeClass> &other) const;
};

template <typename NodeClass>
node_set<NodeClass>
node_set<NodeClass>::union_ (const node_set<NodeClass> &other) const
{
  node_set<NodeClass> result (*this);
  for (auto iter : other)
    result.add (iter);
  return result;
}

template <typename NodeClass>
inline bool
node_set<NodeClass>::operator< (const node_set<NodeClass> &other) const
{
  if (this->size () != other.size ())
    return this->size () < other.size ();
  auto this_iter = this->begin ();
  auto other_iter = other.begin ();
  for (; this_iter != this->end (); ++this_iter, ++other_iter)
    if (*this_iter != *other_iter)
      return (*this_iter)->get_id () < (*other_iter)->get_id ();
  return false;
}

/* A bundle of state for generating a hierarchical dump of one
   of more symbolic::node instances.  */

struct node_dump_state : public text_art::dump_widget_info
{
  node_dump_state (text_art::style_manager &sm,
		   const text_art::theme &theme,
		   text_art::style::id_t tree_style_id,
		   bool show_ids,
		   bool elide_repeated_nodes)
  : dump_widget_info (sm, theme, tree_style_id),
    m_show_ids (show_ids),
    m_elide_repeated_nodes (elide_repeated_nodes)
  {
  }

  /* Whether to show node IDs in dumps.
     - True is useful for debugging
     - false is useful for repeatable unit tests of dumping.  */
  bool m_show_ids;

  /* If true, then if a node appears more than once in a dump,
     only show its descendents the first time, showing a summary of its
     children for repeated occurrences.  */
  bool m_elide_repeated_nodes;

  /* State for implementing m_elide_repeated_nodes.  */
  node_set<symbolic::node> m_nodes_seen;
};

/* Dump a tree-like representation of OBJ and its constituent symbolic::nodes
   to PP.

   For example:
   . (gdb) call index_sval->dump()
   . (27): ‘int’: initial_svalue
   . ╰─ m_reg: (26): ‘int’: decl_region(‘x_10(D)’)
   .    ╰─ parent: (9): frame_region(‘test_bitmask_2’, index: 0, depth: 1)
   .       ╰─ parent: (1): stack region
   .          ╰─ parent: (0): root region

   Note that OBJ is not necessarily itself a symbolic::node, it just needs
   to implement make_dump_widget.

   Set show_ids to false for generating repeatable output (e.g. for unit
   testing of dump output).  */

template <typename T>
void dump_node_tree_to_pp (const T &obj,
			   const text_art::theme *theme,
			   pretty_printer *pp,
			   bool show_ids,
			   bool elide_repeated_nodes)
{
  if (!theme)
    return;

  using namespace text_art;

  style_manager sm;
  style tree_style (get_style_from_color_cap_name ("note"));

  style::id_t tree_style_id (sm.get_or_create_id (tree_style));

  node_dump_state nds (sm, *theme, tree_style_id,
		       show_ids, elide_repeated_nodes);
  if (std::unique_ptr<widget> w = obj.make_dump_widget (nds))
    {
      text_art::canvas c (w->to_canvas (nds.m_sm));
      c.print_to_pp (pp);
    }
}

/* Dump OBJ to OUTF, using OBJ's make_dump_widget member function,
   global_dc's color/theme options, with symbol IDs shown,
   and eliding repeated occurrences of nodes.  */

template <typename T>
void dump_node_tree_to_file (const T &obj, FILE *outf)
{
  tree_dump_pretty_printer pp (outf);
  const text_art::theme *theme = global_dc->get_diagram_theme ();
  dump_node_tree_to_pp<T> (obj, theme, &pp, true, true);
}

/* Dump a tree-like representation of OBJ and its constituent symbolic::nodes
   to stderr, using global_dc's colorization and theming options, with
   symbolic::node IDs shown.

   For example:
   . (gdb) call reg->dump()
   . (26): ‘int’: decl_region(‘x_10(D)’)
   . ╰─ parent: (9): frame_region(‘test_bitmask_2’, index: 0, depth: 1)
   .    ╰─ parent: (1): stack region
   .       ╰─ parent: (0): root region

   Note that OBJ is not necessarily itself a symbolic::node, it just needs
   to implement make_dump_widget.  */

template <typename T>
void dump_node_tree_to_stderr (const T &obj)
{
  dump_node_tree_to_file<T> (obj, stderr);
}

/* As dump_node_tree_to_pp, but write to LOG, indenting the whole output
   according to the current nesting level of the log.  */

template <typename T>
void dump_node_tree_to_logger (const T &obj,
			       logger &log,
			       const text_art::theme *theme,
			       bool show_ids,
			       bool elide_repeated_nodes)
{
  if (!theme)
    return;

  using namespace text_art;

  style_manager sm;
  style tree_style (get_style_from_color_cap_name ("note"));

  style::id_t tree_style_id (sm.get_or_create_id (tree_style));

  node_dump_state nds (sm, *theme, tree_style_id,
		       show_ids, elide_repeated_nodes);
  if (std::unique_ptr<widget> w = obj.make_dump_widget (nds))
    {
      text_art::canvas c (w->to_canvas (nds.m_sm));
      log.log_canvas (c);
    }
}

/* As above, but pass LABEL to OBJ's make_dump_widget call.  */

template <typename T>
void dump_node_tree_to_logger (const T &obj,
			       logger &log,
			       const text_art::theme *theme,
			       const char *label,
			       bool show_ids,
			       bool elide_repeated_nodes)
{
  if (!theme)
    return;

  using namespace text_art;

  style_manager sm;
  style tree_style (get_style_from_color_cap_name ("note"));

  style::id_t tree_style_id (sm.get_or_create_id (tree_style));

  node_dump_state nds (sm, *theme, tree_style_id,
		       show_ids, elide_repeated_nodes);
  if (std::unique_ptr<widget> w = obj.make_dump_widget (nds, label))
    {
      text_art::canvas c (w->to_canvas (nds.m_sm));
      log.log_canvas (c);
    }
}

/* Dump a tree-like representation of OBJ and its constituent symbolic::nodes
   to LOG with symbol IDs shown.  */

template <typename T>
void dump_node_tree_to_logger (const T &obj,
			       logger &log)
{
  dump_node_tree_to_logger<T> (obj, log, log.get_text_art_theme (),
			       true, true);
}

/* As above, but pass LABEL to OBJ's make_dump_widget call.  */

template <typename T>
void dump_node_tree_to_logger (const T &obj,
			       logger &log,
			       const char *label)
{
  dump_node_tree_to_logger<T> (obj, log, log.get_text_art_theme (),
			       label, true, true);
}

class value_node;

} // namespace symbolic

using value_set = symbolic::node_set<symbolic::value_node>;

namespace symbolic {

/* Intermediate base class for values that can be live or dead.  */

class value_node : public node
{
public:
  virtual bool implicitly_live_p (liveness_context &liveness_ctxt) const;

  virtual const svalue *
  dyn_cast_svalue () const { return nullptr; }

  virtual const bit_vector *
  dyn_cast_bit_vector () const { return nullptr; }

  virtual iter_result
  for_each_svalue (iter_result (*cb) (const svalue &sval,
				      void *user_data),
		   void *user_data) const = 0;

protected:
  value_node (complexity c, unsigned id)
  : node (c, id)
  {}

  std::unique_ptr<json::object>
  make_json_obj (const char *type_name) const
  {
    return node::make_json_obj (type_name);
  }
};

/* Base class for traversing the nodes in a node hierarchy
   Subclasses should chain up the method calls. so that visit_node
   is called for all of them.
   visit_* methods should return true iff the traversal should continue
   into child nodes.  */

class node_visitor
{
public:
  virtual ~node_visitor () = default;

  virtual bool
  visit_node (const node &)
  {
    // No-op
    return true;
  }

  virtual bool
  visit_bit_contents (const bit_contents &n);

  virtual bool
  visit_bit_vector (const bit_vector &);

  virtual bool
  visit_region (const region &);

  virtual bool
  visit_scalar_unitless (const scalar<integer> &);

  virtual bool
  visit_scalar_bit (const scalar<integer_with_unit<units::bit>> &);

  virtual bool
  visit_scalar_byte (const scalar<integer_with_unit<units::byte>> &n);

  virtual bool
  visit_svalue (const svalue &n);
};

/* A node_visitor subclass that calls a callback on any value_node
   instances.  */

class value_node_visitor : public node_visitor
{
public:
  value_node_visitor (bool (*cb) (const value_node &val,
				  void *user_data),
		      void *user_data)
    : m_cb (cb),
      m_user_data (user_data)
  {
  }

  bool visit_bit_vector (const bit_vector &bv) final override;
  bool visit_svalue (const svalue &sval) final override;

private:
  bool (*m_cb) (const value_node &val, void *user_data);
  void *m_user_data;
};

/* A (base region, baseline_state) pair for identifying a
   baseline bit_contents or baseline bit_vector.  */

struct baseline_key
{
  const region *m_base_reg;
  baseline_state m_baseline_state;

  bool
  operator< (const baseline_key &other) const;

  bool
  region_has_meaningful_size_p (symbolic::node_manager &node_mgr) const;

  std::unique_ptr<json::object>
  to_json () const;
};

} // namespace ana::symbolic

/* A bundle of state for purging information from a program_state about
   a conjured_bit_vector or conjured_bit_contents.  We pass this whenever
   we could need to create one of these, so that if the program_state already
   has information about this conjured value_node on an execution path, we
   can purge that information, to avoid the analyzer confusing the two
   interpretations of the value as being the same.  */

class conjured_purge
{
public:
  conjured_purge (region_model *model, region_model_context *ctxt)
  : m_model (model), m_ctxt (ctxt)
  {
  }
  void purge (const symbolic::conjured_bit_vector &bv) const;
  void purge (const symbolic::conjured_bit_contents &bc) const;

private:
  region_model *m_model;
  region_model_context *m_ctxt;
};

#if CHECKING_P

namespace selftest {

/* Implementation detail of ASSERT_SYMBOLIC_NODE_DUMP_EQ.  */

void
assert_symbolic_node_dump_eq (const ::selftest::location &loc,
			      const symbolic::node &node,
			      const char *expected_str);

/* Assert that dumping NODE results in EXPECTED_STR.  */

#define ASSERT_SYMBOLIC_NODE_DUMP_EQ(NODE, EXPECTED_STR) \
  SELFTEST_BEGIN_STMT						\
  assert_symbolic_node_dump_eq (SELFTEST_LOCATION, NODE,	\
				EXPECTED_STR);			\
  SELFTEST_END_STMT

/* Implementation details of ASSERT_SAME_SYMBOLIC_NODE.  */

void
assert_same_symbolic_node (const ::selftest::location &loc,
			   const char *desc_lhs,
			   const char *desc_rhs,
			   const symbolic::node *lhs,
			   const symbolic::node *rhs);

inline void
assert_same_symbolic_node (const ::selftest::location &loc,
			   const char *desc_lhs,
			   const char *desc_rhs,
			   const symbolic::node &lhs,
			   const symbolic::node &rhs)
{
  assert_same_symbolic_node (loc,
			     desc_lhs,
			     desc_rhs,
			     &lhs,
			     &rhs);
}

/* Assert that LHS and RHS are the same symbolic::node instance.  */

#define ASSERT_SAME_SYMBOLIC_NODE(LHS, RHS)				\
  SELFTEST_BEGIN_STMT						\
  assert_same_symbolic_node (SELFTEST_LOCATION, (#LHS), (#RHS), (LHS), (RHS));	\
  SELFTEST_END_STMT

#define ASSERT_SAME_SYMBOLIC_NODE_AT(LOC, LHS, RHS)			\
  SELFTEST_BEGIN_STMT						\
  assert_same_symbolic_node ((LOC), (#LHS), (#RHS), (LHS), (RHS));	\
  SELFTEST_END_STMT

/* Implementation details of ASSERT_DIFFERENT_SYMBOLIC_NODE.  */

void
assert_different_symbolic_node (const ::selftest::location &loc,
				const char *desc_lhs,
				const char *desc_rhs,
				const symbolic::node *lhs,
				const symbolic::node *rhs);

inline void
assert_different_symbolic_node (const ::selftest::location &loc,
				const char *desc_lhs,
				const char *desc_rhs,
				const symbolic::node &lhs,
				const symbolic::node &rhs)
{
  assert_different_symbolic_node (loc,
				  desc_lhs,
				  desc_rhs,
				  &lhs,
				  &rhs);
}

/* Assert that LHS and RHS are different symbolic::node instances.  */

#define ASSERT_DIFFERENT_SYMBOLIC_NODE(LHS, RHS)				\
  SELFTEST_BEGIN_STMT						\
  assert_different_symbolic_node (SELFTEST_LOCATION, (#LHS), (#RHS), (LHS), (RHS));	\
  SELFTEST_END_STMT

/* Implementation detail of ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ.  */

void
assert_symbolic_node_markup_text_eq (const ::selftest::location &loc,
				     const symbolic::node &node,
				     const diagnostics::markup::lowering_options &,
				     const char *expected_str);

/* Assert that NODE's make_markup_node's result when lowered to plain
   text is EXPECTED_STR.  */

#define ASSERT_SYMBOLIC_NODE_MARKUP_TEXT_EQ(NODE, LOWERING_OPTS, EXPECTED_STR) \
  SELFTEST_BEGIN_STMT							\
  assert_symbolic_node_markup_text_eq (SELFTEST_LOCATION, (NODE),		\
				(LOWERING_OPTS), (EXPECTED_STR));	\
  SELFTEST_END_STMT

} // namespace ana::selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* GCC_ANALYZER_SYMBOLIC_NODES_H */
