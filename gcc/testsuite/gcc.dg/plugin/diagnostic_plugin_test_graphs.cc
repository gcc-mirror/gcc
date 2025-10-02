/* This plugin exercises diagnostic graphs.
   We emit an error with a pair of digraphs associated with it,
   and a global digraph showing the optimization passes.  */

#define INCLUDE_MAP
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stringpool.h"
#include "toplev.h"
#include "basic-block.h"
#include "hash-table.h"
#include "vec.h"
#include "ggc.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "plugin-version.h"
#include "diagnostic.h"
#include "context.h"
#include "gcc-rich-location.h"
#include "diagnostics/metadata.h"
#include "diagnostics/digraphs.h"
#include "pass_manager.h"


int plugin_is_GPL_compatible;

const pass_data pass_data_test_graph_emission =
{
  GIMPLE_PASS, /* type */
  "test_graph_emission", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_test_graph_emission : public gimple_opt_pass
{
public:
  pass_test_graph_emission(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_test_graph_emission, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) { return true; }
  virtual unsigned int execute (function *);

}; // class pass_test_graph_emission

/* Determine if STMT is a call with NUM_ARGS arguments to a function
   named FUNCNAME.
   If so, return STMT as a gcall *.  Otherwise return NULL.  */

static gcall *
check_for_named_call (gimple *stmt,
		      const char *funcname, unsigned int num_args)
{
  gcc_assert (funcname);

  gcall *call = dyn_cast <gcall *> (stmt);
  if (!call)
    return NULL;

  tree fndecl = gimple_call_fndecl (call);
  if (!fndecl)
    return NULL;

  if (strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), funcname))
    return NULL;

  if (gimple_call_num_args (call) != num_args)
    {
      error_at (stmt->location, "expected number of args: %i (got %i)",
		num_args, gimple_call_num_args (call));
      return NULL;
    }

  return call;
}

class lazy_passes_graph : public lazily_created<diagnostics::digraphs::digraph>
{
public:
  lazy_passes_graph (const ::gcc::pass_manager &pass_manager_)
  : m_pass_manager (pass_manager_)
  {
  }

private:
  std::unique_ptr<diagnostics::digraphs::digraph>
  create_object () const final override
  {
    auto g = std::make_unique<diagnostics::digraphs::digraph> ();
    g->set_description ("Optimization Passes");

#define DEF_PASS_LIST(NAME)					\
    add_top_level_pass_list (*g, #NAME, m_pass_manager.NAME);

    GCC_PASS_LISTS

#undef DEF_PASS_LIST

    return g;
  }

  void
  add_top_level_pass_list (diagnostics::digraphs::digraph &g,
			   const char *pass_list_name,
			   const opt_pass *p) const
  {
    gcc_assert (p);
    auto n = std::make_unique<diagnostics::digraphs::node> (g, pass_list_name);
    n->set_label (pass_list_name);
    add_child_pass (g, *n, *p);
    g.add_node (std::move (n));
  }

  diagnostics::digraphs::node &
  add_child_pass (diagnostics::digraphs::digraph &g,
		  diagnostics::digraphs::node &parent_node,
		  const opt_pass &p) const
  {
    std::string node_label;
    std::string node_id;
    if (p.static_pass_number > 0 )
      {
	node_label = std::to_string (p.static_pass_number) + "_" + p.name;
	node_id = node_label;
      }
    else
      {
	node_label = std::string (p.name);
	pretty_printer pp;
	pp_printf (&pp, "%s_%p", p.name, &p);
	node_id = pp_formatted_text (&pp);
      }
    auto n
      = std::make_unique<diagnostics::digraphs::node> (g,
						       std::move (node_id));
    n->set_label (node_label.c_str ());
    diagnostics::digraphs::node &result = *n;
    parent_node.add_child (std::move (n));

    // TODO: add attrs for things like type, properties_*, etc

    if (p.sub)
      {
	auto &other_node = add_child_pass (g, parent_node, *p.sub);
	g.add_edge (nullptr, result, other_node, "sub");
      }

    if (p.next)
      {
	auto &other_node = add_child_pass (g, parent_node, *p.next);
	g.add_edge (nullptr, result, other_node, "next");
      }

    return result;
  }

  const ::gcc::pass_manager &m_pass_manager;
};

static void
report_diag_with_graphs (location_t loc)
{
  class my_lazy_digraphs : public diagnostics::metadata::lazy_digraphs
  {
  public:
    using diagnostic_graph = diagnostics::digraphs::digraph;
    using diagnostic_node = diagnostics::digraphs::node;
    using diagnostic_edge = diagnostics::digraphs::edge;

    std::unique_ptr<std::vector<std::unique_ptr<diagnostic_graph>>>
    create_object () const final override
    {
      auto graphs
	= std::make_unique<std::vector<std::unique_ptr<diagnostic_graph>>> ();

      graphs->push_back (make_test_graph ("foo"));
      graphs->push_back (make_test_graph ("bar"));

      return graphs;
    }

  private:
    std::unique_ptr<diagnostic_graph>
    make_test_graph (const char *desc) const
    {
      auto g = std::make_unique<diagnostic_graph> ();
      g->set_description (desc);
      auto a = std::make_unique<diagnostic_node> (*g, "a");
      auto b = std::make_unique<diagnostic_node> (*g, "b");
      const json::string_property color ("/placeholder-prefix/color");
      b->set_property (color, "red");
      auto c = std::make_unique<diagnostic_node> (*g, "c");
      c->set_label ("I am a node label");

      auto e = std::make_unique<diagnostic_edge> (*g, "my-edge", *a, *c);
      e->set_label ("I am an edge label");
      g->add_edge (std::move (e));

      g->add_node (std::move (a));

      b->add_child (std::move (c));
      g->add_node (std::move (b));

      return g;
    }
  };

  gcc_rich_location rich_loc (loc);
  diagnostics::metadata meta;
  my_lazy_digraphs ldg;
  meta.set_lazy_digraphs (&ldg);
  error_meta (&rich_loc, meta,
	      "this is a placeholder error, with graphs");
}

/* Exercise diagnostic graph emission.  */

unsigned int
pass_test_graph_emission::execute (function *fun)
{
  gimple_stmt_iterator gsi;
  basic_block bb;

  FOR_EACH_BB_FN (bb, fun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);

	if (gcall *call = check_for_named_call (stmt, "here", 0))
	  report_diag_with_graphs (gimple_location (call));
      }

  return 0;
}

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  struct register_pass_info pass_info;
  const char *plugin_name = plugin_info->base_name;
  int argc = plugin_info->argc;
  struct plugin_argument *argv = plugin_info->argv;

  if (!plugin_default_version_check (version, &gcc_version))
    return 1;

  pass_info.pass = new pass_test_graph_emission (g);
  pass_info.reference_pass_name = "ssa";
  pass_info.ref_pass_instance_number = 1;
  pass_info.pos_op = PASS_POS_INSERT_AFTER;
  register_callback (plugin_name, PLUGIN_PASS_MANAGER_SETUP, NULL,
		     &pass_info);

  gcc_assert (::g->get_passes ());
  global_dc->report_global_digraph (lazy_passes_graph (*::g->get_passes ()));

  return 0;
}
