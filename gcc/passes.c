/* Top level of GCC compilers (cc1, cc1plus, etc.)
   Copyright (C) 1987-2018 Free Software Foundation, Inc.

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

/* This is the top level of cc1/c++.
   It parses command args, opens files, invokes the various passes
   in the proper order, and counts the time used by each.
   Error messages and low-level interface to malloc also handled here.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "ssa.h"
#include "emit-rtl.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "fold-const.h"
#include "varasm.h"
#include "output.h"
#include "graph.h"
#include "debug.h"
#include "cfgloop.h"
#include "value-prof.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "tree-pass.h"
#include "plugin.h"
#include "ipa-utils.h"
#include "tree-pretty-print.h" /* for dump_function_header */
#include "context.h"
#include "pass_manager.h"
#include "cfgrtl.h"
#include "tree-ssa-live.h"  /* For remove_unused_locals.  */
#include "tree-cfgcleanup.h"
#include "insn-addr.h" /* for INSN_ADDRESSES_ALLOC.  */
#include "diagnostic-core.h" /* for fnotice */
#include "stringpool.h"
#include "attribs.h"

using namespace gcc;

/* This is used for debugging.  It allows the current pass to printed
   from anywhere in compilation.
   The variable current_pass is also used for statistics and plugins.  */
opt_pass *current_pass;

/* Most passes are single-instance (within their context) and thus don't
   need to implement cloning, but passes that support multiple instances
   *must* provide their own implementation of the clone method.

   Handle this by providing a default implemenation, but make it a fatal
   error to call it.  */

opt_pass *
opt_pass::clone ()
{
  internal_error ("pass %s does not support cloning", name);
}

void
opt_pass::set_pass_param (unsigned int, bool)
{
  internal_error ("pass %s needs a set_pass_param implementation to handle the"
		  " extra argument in NEXT_PASS", name);
}

bool
opt_pass::gate (function *)
{
  return true;
}

unsigned int
opt_pass::execute (function *)
{
  return 0;
}

opt_pass::opt_pass (const pass_data &data, context *ctxt)
  : pass_data (data),
    sub (NULL),
    next (NULL),
    static_pass_number (0),
    m_ctxt (ctxt)
{
}


void
pass_manager::execute_early_local_passes ()
{
  execute_pass_list (cfun, pass_build_ssa_passes_1->sub);
  execute_pass_list (cfun, pass_local_optimization_passes_1->sub);
}

unsigned int
pass_manager::execute_pass_mode_switching ()
{
  return pass_mode_switching_1->execute (cfun);
}


/* Call from anywhere to find out what pass this is.  Useful for
   printing out debugging information deep inside an service
   routine.  */
void
print_current_pass (FILE *file)
{
  if (current_pass)
    fprintf (file, "current pass = %s (%d)\n",
	     current_pass->name, current_pass->static_pass_number);
  else
    fprintf (file, "no current pass.\n");
}


/* Call from the debugger to get the current pass name.  */
DEBUG_FUNCTION void
debug_pass (void)
{
  print_current_pass (stderr);
}



/* Global variables used to communicate with passes.  */
bool in_gimple_form;


/* This is called from various places for FUNCTION_DECL, VAR_DECL,
   and TYPE_DECL nodes.

   This does nothing for local (non-static) variables, unless the
   variable is a register variable with DECL_ASSEMBLER_NAME set.  In
   that case, or if the variable is not an automatic, it sets up the
   RTL and outputs any assembler code (label definition, storage
   allocation and initialization).

   DECL is the declaration.  TOP_LEVEL is nonzero
   if this declaration is not within a function.  */

void
rest_of_decl_compilation (tree decl,
			  int top_level,
			  int at_end)
{
  bool finalize = true;

  /* We deferred calling assemble_alias so that we could collect
     other attributes such as visibility.  Emit the alias now.  */
  if (!in_lto_p)
  {
    tree alias;
    alias = lookup_attribute ("alias", DECL_ATTRIBUTES (decl));
    if (alias)
      {
	alias = TREE_VALUE (TREE_VALUE (alias));
	alias = get_identifier (TREE_STRING_POINTER (alias));
	/* A quirk of the initial implementation of aliases required that the
	   user add "extern" to all of them.  Which is silly, but now
	   historical.  Do note that the symbol is in fact locally defined.  */
	DECL_EXTERNAL (decl) = 0;
	TREE_STATIC (decl) = 1;
	assemble_alias (decl, alias);
	finalize = false;
      }
  }

  /* Can't defer this, because it needs to happen before any
     later function definitions are processed.  */
  if (HAS_DECL_ASSEMBLER_NAME_P (decl)
      && DECL_ASSEMBLER_NAME_SET_P (decl)
      && DECL_REGISTER (decl))
    make_decl_rtl (decl);

  /* Forward declarations for nested functions are not "external",
     but we need to treat them as if they were.  */
  if (TREE_STATIC (decl) || DECL_EXTERNAL (decl)
      || TREE_CODE (decl) == FUNCTION_DECL)
    {
      timevar_push (TV_VARCONST);

      /* Don't output anything when a tentative file-scope definition
	 is seen.  But at end of compilation, do output code for them.

	 We do output all variables and rely on
	 callgraph code to defer them except for forward declarations
	 (see gcc.c-torture/compile/920624-1.c) */
      if ((at_end
	   || !DECL_DEFER_OUTPUT (decl)
	   || DECL_INITIAL (decl))
	  && (!VAR_P (decl) || !DECL_HAS_VALUE_EXPR_P (decl))
	  && !DECL_EXTERNAL (decl))
	{
	  /* When reading LTO unit, we also read varpool, so do not
	     rebuild it.  */
	  if (in_lto_p && !at_end)
	    ;
	  else if (finalize && TREE_CODE (decl) != FUNCTION_DECL)
	    varpool_node::finalize_decl (decl);
	}

#ifdef ASM_FINISH_DECLARE_OBJECT
      if (decl == last_assemble_variable_decl)
	{
	  ASM_FINISH_DECLARE_OBJECT (asm_out_file, decl,
				     top_level, at_end);
	}
#endif

      /* Now that we have activated any function-specific attributes
	 that might affect function decl, particularly align, relayout it.  */
      if (TREE_CODE (decl) == FUNCTION_DECL)
	targetm.target_option.relayout_function (decl);

      timevar_pop (TV_VARCONST);
    }
  else if (TREE_CODE (decl) == TYPE_DECL
	   /* Like in rest_of_type_compilation, avoid confusing the debug
	      information machinery when there are errors.  */
	   && !seen_error ())
    {
      timevar_push (TV_SYMOUT);
      debug_hooks->type_decl (decl, !top_level);
      timevar_pop (TV_SYMOUT);
    }

  /* Let cgraph know about the existence of variables.  */
  if (in_lto_p && !at_end)
    ;
  else if (VAR_P (decl) && !DECL_EXTERNAL (decl)
	   && TREE_STATIC (decl))
    varpool_node::get_create (decl);

  /* Generate early debug for global variables.  Any local variables will
     be handled by either handling reachable functions from
     finalize_compilation_unit (and by consequence, locally scoped
     symbols), or by rest_of_type_compilation below.

     For Go's hijack of the debug_hooks to implement -fdump-go-spec, pick up
     function prototypes.  Go's debug_hooks will not forward them to the
     wrapped hooks.  */
  if (!in_lto_p
      && (TREE_CODE (decl) != FUNCTION_DECL
	  /* This will pick up function prototypes with no bodies,
	     which are not visible in finalize_compilation_unit()
	     while iterating with FOR_EACH_*_FUNCTION through the
	     symbol table.  */
	  || (flag_dump_go_spec != NULL
	      && !DECL_SAVED_TREE (decl)
	      && DECL_STRUCT_FUNCTION (decl) == NULL))

      /* We need to check both decl_function_context and
	 current_function_decl here to make sure local extern
	 declarations end up with the correct context.

	 For local extern declarations, decl_function_context is
	 empty, but current_function_decl is set to the function where
	 the extern was declared .  Without the check for
	 !current_function_decl below, the local extern ends up
	 incorrectly with a top-level context.

	 For example:

	 namespace S
	 {
	   int
	   f()
	   {
	     {
	       int i = 42;
	       {
	         extern int i; // Local extern declaration.
		 return i;
	       }
	     }
	   }
	 }
      */
      && !decl_function_context (decl)
      && !current_function_decl
      && DECL_SOURCE_LOCATION (decl) != BUILTINS_LOCATION
      && (!decl_type_context (decl)
	  /* If we created a varpool node for the decl make sure to
	     call early_global_decl.  Otherwise we miss changes
	     introduced by member definitions like
		struct A { static int staticdatamember; };
		int A::staticdatamember;
	     and thus have incomplete early debug and late debug
	     called from varpool node removal fails to handle it
	     properly.  */
	  || (finalize
	      && VAR_P (decl)
	      && TREE_STATIC (decl) && !DECL_EXTERNAL (decl)))
      /* Avoid confusing the debug information machinery when there are
	 errors.  */
      && !seen_error ())
    (*debug_hooks->early_global_decl) (decl);
}

/* Called after finishing a record, union or enumeral type.  */

void
rest_of_type_compilation (tree type, int toplev)
{
  /* Avoid confusing the debug information machinery when there are
     errors.  */
  if (seen_error ())
    return;

  timevar_push (TV_SYMOUT);
  debug_hooks->type_decl (TYPE_STUB_DECL (type), !toplev);
  timevar_pop (TV_SYMOUT);
}



void
pass_manager::
finish_optimization_passes (void)
{
  int i;
  struct dump_file_info *dfi;
  char *name;
  gcc::dump_manager *dumps = m_ctxt->get_dumps ();

  timevar_push (TV_DUMP);
  if (profile_arc_flag || flag_test_coverage || flag_branch_probabilities)
    {
      dumps->dump_start (pass_profile_1->static_pass_number, NULL);
      end_branch_prob ();
      dumps->dump_finish (pass_profile_1->static_pass_number);
    }

  if (optimize > 0)
    {
      dumps->dump_start (pass_profile_1->static_pass_number, NULL);
      print_combine_total_stats ();
      dumps->dump_finish (pass_profile_1->static_pass_number);
    }

  /* Do whatever is necessary to finish printing the graphs.  */
  for (i = TDI_end; (dfi = dumps->get_dump_file_info (i)) != NULL; ++i)
    if (dfi->graph_dump_initialized)
      {
	name = dumps->get_dump_file_name (dfi);
	finish_graph_dump_file (name);
	free (name);
      }

  timevar_pop (TV_DUMP);
}

static unsigned int
execute_build_ssa_passes (void)
{
  /* Once this pass (and its sub-passes) are complete, all functions
     will be in SSA form.  Technically this state change is happening
     a tad early, since the sub-passes have not yet run, but since
     none of the sub-passes are IPA passes and do not create new
     functions, this is ok.  We're setting this value for the benefit
     of IPA passes that follow.  */
  if (symtab->state < IPA_SSA)
    symtab->state = IPA_SSA;
  return 0;
}

namespace {

const pass_data pass_data_build_ssa_passes =
{
  SIMPLE_IPA_PASS, /* type */
  "build_ssa_passes", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_EARLY_LOCAL, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  /* todo_flags_finish is executed before subpases. For this reason
     it makes no sense to remove unreachable functions here.  */
  0, /* todo_flags_finish */
};

class pass_build_ssa_passes : public simple_ipa_opt_pass
{
public:
  pass_build_ssa_passes (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_build_ssa_passes, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      /* Don't bother doing anything if the program has errors.  */
      return (!seen_error () && !in_lto_p);
    }

  virtual unsigned int execute (function *)
    {
      return execute_build_ssa_passes ();
    }

}; // class pass_build_ssa_passes

const pass_data pass_data_local_optimization_passes =
{
  SIMPLE_IPA_PASS, /* type */
  "opt_local_passes", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_local_optimization_passes : public simple_ipa_opt_pass
{
public:
  pass_local_optimization_passes (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_local_optimization_passes, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      /* Don't bother doing anything if the program has errors.  */
      return (!seen_error () && !in_lto_p);
    }

}; // class pass_local_optimization_passes

} // anon namespace

simple_ipa_opt_pass *
make_pass_build_ssa_passes (gcc::context *ctxt)
{
  return new pass_build_ssa_passes (ctxt);
}

simple_ipa_opt_pass *
make_pass_local_optimization_passes (gcc::context *ctxt)
{
  return new pass_local_optimization_passes (ctxt);
}

namespace {

const pass_data pass_data_all_early_optimizations =
{
  GIMPLE_PASS, /* type */
  "early_optimizations", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_all_early_optimizations : public gimple_opt_pass
{
public:
  pass_all_early_optimizations (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_all_early_optimizations, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return (optimize >= 1
	      /* Don't bother doing anything if the program has errors.  */
	      && !seen_error ());
    }

}; // class pass_all_early_optimizations

} // anon namespace

static gimple_opt_pass *
make_pass_all_early_optimizations (gcc::context *ctxt)
{
  return new pass_all_early_optimizations (ctxt);
}

namespace {

const pass_data pass_data_all_optimizations =
{
  GIMPLE_PASS, /* type */
  "*all_optimizations", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_OPTIMIZE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_all_optimizations : public gimple_opt_pass
{
public:
  pass_all_optimizations (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_all_optimizations, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return optimize >= 1 && !optimize_debug; }

}; // class pass_all_optimizations

} // anon namespace

static gimple_opt_pass *
make_pass_all_optimizations (gcc::context *ctxt)
{
  return new pass_all_optimizations (ctxt);
}

namespace {

const pass_data pass_data_all_optimizations_g =
{
  GIMPLE_PASS, /* type */
  "*all_optimizations_g", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_OPTIMIZE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_all_optimizations_g : public gimple_opt_pass
{
public:
  pass_all_optimizations_g (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_all_optimizations_g, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return optimize >= 1 && optimize_debug; }

}; // class pass_all_optimizations_g

} // anon namespace

static gimple_opt_pass *
make_pass_all_optimizations_g (gcc::context *ctxt)
{
  return new pass_all_optimizations_g (ctxt);
}

namespace {

const pass_data pass_data_rest_of_compilation =
{
  RTL_PASS, /* type */
  "*rest_of_compilation", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_REST_OF_COMPILATION, /* tv_id */
  PROP_rtl, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_rest_of_compilation : public rtl_opt_pass
{
public:
  pass_rest_of_compilation (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rest_of_compilation, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      /* Early return if there were errors.  We can run afoul of our
	 consistency checks, and there's not really much point in fixing them.  */
      return !(rtl_dump_and_exit || flag_syntax_only || seen_error ());
    }

}; // class pass_rest_of_compilation

} // anon namespace

static rtl_opt_pass *
make_pass_rest_of_compilation (gcc::context *ctxt)
{
  return new pass_rest_of_compilation (ctxt);
}

namespace {

const pass_data pass_data_postreload =
{
  RTL_PASS, /* type */
  "*all-postreload", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_POSTRELOAD, /* tv_id */
  PROP_rtl, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_postreload : public rtl_opt_pass
{
public:
  pass_postreload (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_postreload, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return reload_completed; }

}; // class pass_postreload

} // anon namespace

static rtl_opt_pass *
make_pass_postreload (gcc::context *ctxt)
{
  return new pass_postreload (ctxt);
}

namespace {

const pass_data pass_data_late_compilation =
{
  RTL_PASS, /* type */
  "*all-late_compilation", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_LATE_COMPILATION, /* tv_id */
  PROP_rtl, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_late_compilation : public rtl_opt_pass
{
public:
  pass_late_compilation (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_late_compilation, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
  {
    return reload_completed || targetm.no_register_allocation;
  }

}; // class pass_late_compilation

} // anon namespace

static rtl_opt_pass *
make_pass_late_compilation (gcc::context *ctxt)
{
  return new pass_late_compilation (ctxt);
}



/* Set the static pass number of pass PASS to ID and record that
   in the mapping from static pass number to pass.  */

void
pass_manager::
set_pass_for_id (int id, opt_pass *pass)
{
  pass->static_pass_number = id;
  if (passes_by_id_size <= id)
    {
      passes_by_id = XRESIZEVEC (opt_pass *, passes_by_id, id + 1);
      memset (passes_by_id + passes_by_id_size, 0,
	      (id + 1 - passes_by_id_size) * sizeof (void *));
      passes_by_id_size = id + 1;
    }
  passes_by_id[id] = pass;
}

/* Return the pass with the static pass number ID.  */

opt_pass *
pass_manager::get_pass_for_id (int id) const
{
  if (id >= passes_by_id_size)
    return NULL;
  return passes_by_id[id];
}

/* Iterate over the pass tree allocating dump file numbers.  We want
   to do this depth first, and independent of whether the pass is
   enabled or not.  */

void
register_one_dump_file (opt_pass *pass)
{
  g->get_passes ()->register_one_dump_file (pass);
}

void
pass_manager::register_one_dump_file (opt_pass *pass)
{
  char *dot_name, *flag_name, *glob_name;
  const char *name, *full_name, *prefix;

  /* Buffer big enough to format a 32-bit UINT_MAX into.  */
  char num[11];
  dump_kind dkind;
  int id;
  optgroup_flags_t optgroup_flags = OPTGROUP_NONE;
  gcc::dump_manager *dumps = m_ctxt->get_dumps ();

  /* See below in next_pass_1.  */
  num[0] = '\0';
  if (pass->static_pass_number != -1)
    sprintf (num, "%u", ((int) pass->static_pass_number < 0
			 ? 1 : pass->static_pass_number));

  /* The name is both used to identify the pass for the purposes of plugins,
     and to specify dump file name and option.
     The latter two might want something short which is not quite unique; for
     that reason, we may have a disambiguating prefix, followed by a space
     to mark the start of the following dump file name / option string.  */
  name = strchr (pass->name, ' ');
  name = name ? name + 1 : pass->name;
  dot_name = concat (".", name, num, NULL);
  if (pass->type == SIMPLE_IPA_PASS || pass->type == IPA_PASS)
    {
      prefix = "ipa-";
      dkind = DK_ipa;
      optgroup_flags |= OPTGROUP_IPA;
    }
  else if (pass->type == GIMPLE_PASS)
    {
      prefix = "tree-";
      dkind = DK_tree;
    }
  else
    {
      prefix = "rtl-";
      dkind = DK_rtl;
    }

  flag_name = concat (prefix, name, num, NULL);
  glob_name = concat (prefix, name, NULL);
  optgroup_flags |= pass->optinfo_flags;
  /* For any passes that do not have an optgroup set, and which are not
     IPA passes setup above, set the optgroup to OPTGROUP_OTHER so that
     any dump messages are emitted properly under -fopt-info(-optall).  */
  if (optgroup_flags == OPTGROUP_NONE)
    optgroup_flags = OPTGROUP_OTHER;
  id = dumps->dump_register (dot_name, flag_name, glob_name, dkind,
			     optgroup_flags,
			     true);
  set_pass_for_id (id, pass);
  full_name = concat (prefix, pass->name, num, NULL);
  register_pass_name (pass, full_name);
  free (CONST_CAST (char *, full_name));
}

/* Register the dump files for the pass_manager starting at PASS. */

void
pass_manager::register_dump_files (opt_pass *pass)
{
  do
    {
      if (pass->name && pass->name[0] != '*')
        register_one_dump_file (pass);

      if (pass->sub)
        register_dump_files (pass->sub);

      pass = pass->next;
    }
  while (pass);
}

/* Register PASS with NAME.  */

void
pass_manager::register_pass_name (opt_pass *pass, const char *name)
{
  if (!m_name_to_pass_map)
    m_name_to_pass_map = new hash_map<nofree_string_hash, opt_pass *> (256);

  if (m_name_to_pass_map->get (name))
    return; /* Ignore plugin passes.  */

  const char *unique_name = xstrdup (name);
  m_name_to_pass_map->put (unique_name, pass);
}

/* Map from pass id to canonicalized pass name.  */

typedef const char *char_ptr;
static vec<char_ptr> pass_tab;

/* Callback function for traversing NAME_TO_PASS_MAP.  */

bool
passes_pass_traverse (const char *const &name, opt_pass *const &pass, void *)
{
  gcc_assert (pass->static_pass_number > 0);
  gcc_assert (pass_tab.exists ());

  pass_tab[pass->static_pass_number] = name;

  return 1;
}

/* The function traverses NAME_TO_PASS_MAP and creates a pass info
   table for dumping purpose.  */

void
pass_manager::create_pass_tab (void) const
{
  if (!flag_dump_passes)
    return;

  pass_tab.safe_grow_cleared (passes_by_id_size + 1);
  m_name_to_pass_map->traverse <void *, passes_pass_traverse> (NULL);
}

static bool override_gate_status (opt_pass *, tree, bool);

/* Dump the instantiated name for PASS. IS_ON indicates if PASS
   is turned on or not.  */

static void
dump_one_pass (opt_pass *pass, int pass_indent)
{
  int indent = 3 * pass_indent;
  const char *pn;
  bool is_on, is_really_on;

  is_on = pass->gate (cfun);
  is_really_on = override_gate_status (pass, current_function_decl, is_on);

  if (pass->static_pass_number <= 0)
    pn = pass->name;
  else
    pn = pass_tab[pass->static_pass_number];

  fprintf (stderr, "%*s%-40s%*s:%s%s\n", indent, " ", pn,
           (15 - indent < 0 ? 0 : 15 - indent), " ",
           is_on ? "  ON" : "  OFF",
           ((!is_on) == (!is_really_on) ? ""
            : (is_really_on ? " (FORCED_ON)" : " (FORCED_OFF)")));
}

/* Dump pass list PASS with indentation INDENT.  */

static void
dump_pass_list (opt_pass *pass, int indent)
{
  do
    {
      dump_one_pass (pass, indent);
      if (pass->sub)
        dump_pass_list (pass->sub, indent + 1);
      pass = pass->next;
    }
  while (pass);
}

/* Dump all optimization passes.  */

void
dump_passes (void)
{
  g->get_passes ()->dump_passes ();
}

void
pass_manager::dump_passes () const
{
  push_dummy_function (true);

  create_pass_tab ();

  dump_pass_list (all_lowering_passes, 1);
  dump_pass_list (all_small_ipa_passes, 1);
  dump_pass_list (all_regular_ipa_passes, 1);
  dump_pass_list (all_late_ipa_passes, 1);
  dump_pass_list (all_passes, 1);

  pop_dummy_function ();
}

/* Returns the pass with NAME.  */

opt_pass *
pass_manager::get_pass_by_name (const char *name)
{
  opt_pass **p = m_name_to_pass_map->get (name);
  if (p)
    return *p;

  return NULL;
}


/* Range [start, last].  */

struct uid_range
{
  unsigned int start;
  unsigned int last;
  const char *assem_name;
  struct uid_range *next;
};

typedef struct uid_range *uid_range_p;


static vec<uid_range_p> enabled_pass_uid_range_tab;
static vec<uid_range_p> disabled_pass_uid_range_tab;


/* Parse option string for -fdisable- and -fenable-
   The syntax of the options:

   -fenable-<pass_name>
   -fdisable-<pass_name>

   -fenable-<pass_name>=s1:e1,s2:e2,...
   -fdisable-<pass_name>=s1:e1,s2:e2,...
*/

static void
enable_disable_pass (const char *arg, bool is_enable)
{
  opt_pass *pass;
  char *range_str, *phase_name;
  char *argstr = xstrdup (arg);
  vec<uid_range_p> *tab = 0;

  range_str = strchr (argstr,'=');
  if (range_str)
    {
      *range_str = '\0';
      range_str++;
    }

  phase_name = argstr;
  if (!*phase_name)
    {
      if (is_enable)
        error ("unrecognized option -fenable");
      else
        error ("unrecognized option -fdisable");
      free (argstr);
      return;
    }
  pass = g->get_passes ()->get_pass_by_name (phase_name);
  if (!pass || pass->static_pass_number == -1)
    {
      if (is_enable)
        error ("unknown pass %s specified in -fenable", phase_name);
      else
        error ("unknown pass %s specified in -fdisable", phase_name);
      free (argstr);
      return;
    }

  if (is_enable)
    tab = &enabled_pass_uid_range_tab;
  else
    tab = &disabled_pass_uid_range_tab;

  if ((unsigned) pass->static_pass_number >= tab->length ())
    tab->safe_grow_cleared (pass->static_pass_number + 1);

  if (!range_str)
    {
      uid_range_p slot;
      uid_range_p new_range = XCNEW (struct uid_range);

      new_range->start = 0;
      new_range->last = (unsigned)-1;

      slot = (*tab)[pass->static_pass_number];
      new_range->next = slot;
      (*tab)[pass->static_pass_number] = new_range;
      if (is_enable)
        inform (UNKNOWN_LOCATION, "enable pass %s for functions in the range "
                "of [%u, %u]", phase_name, new_range->start, new_range->last);
      else
        inform (UNKNOWN_LOCATION, "disable pass %s for functions in the range "
                "of [%u, %u]", phase_name, new_range->start, new_range->last);
    }
  else
    {
      char *next_range = NULL;
      char *one_range = range_str;
      char *end_val = NULL;

      do
	{
	  uid_range_p slot;
	  uid_range_p new_range;
	  char *invalid = NULL;
	  long start;
	  char *func_name = NULL;

	  next_range = strchr (one_range, ',');
	  if (next_range)
	    {
	      *next_range = '\0';
	      next_range++;
	    }

	  end_val = strchr (one_range, ':');
	  if (end_val)
	    {
	      *end_val = '\0';
	      end_val++;
	    }
	  start = strtol (one_range, &invalid, 10);
	  if (*invalid || start < 0)
	    {
              if (end_val || (one_range[0] >= '0'
			      && one_range[0] <= '9'))
                {
                  error ("Invalid range %s in option %s",
                         one_range,
                         is_enable ? "-fenable" : "-fdisable");
                  free (argstr);
                  return;
                }
	      func_name = one_range;
	    }
	  if (!end_val)
	    {
	      new_range = XCNEW (struct uid_range);
              if (!func_name)
                {
                  new_range->start = (unsigned) start;
                  new_range->last = (unsigned) start;
                }
              else
                {
                  new_range->start = (unsigned) -1;
                  new_range->last = (unsigned) -1;
                  new_range->assem_name = xstrdup (func_name);
                }
	    }
	  else
	    {
	      long last = strtol (end_val, &invalid, 10);
	      if (*invalid || last < start)
		{
		  error ("Invalid range %s in option %s",
			 end_val,
			 is_enable ? "-fenable" : "-fdisable");
		  free (argstr);
		  return;
		}
	      new_range = XCNEW (struct uid_range);
	      new_range->start = (unsigned) start;
	      new_range->last = (unsigned) last;
	    }

          slot = (*tab)[pass->static_pass_number];
          new_range->next = slot;
          (*tab)[pass->static_pass_number] = new_range;
          if (is_enable)
            {
              if (new_range->assem_name)
                inform (UNKNOWN_LOCATION,
                        "enable pass %s for function %s",
                        phase_name, new_range->assem_name);
              else
                inform (UNKNOWN_LOCATION,
                        "enable pass %s for functions in the range of [%u, %u]",
                        phase_name, new_range->start, new_range->last);
            }
          else
            {
              if (new_range->assem_name)
                inform (UNKNOWN_LOCATION,
                        "disable pass %s for function %s",
                        phase_name, new_range->assem_name);
              else
                inform (UNKNOWN_LOCATION,
                        "disable pass %s for functions in the range of [%u, %u]",
                        phase_name, new_range->start, new_range->last);
            }

	  one_range = next_range;
	} while (next_range);
    }

  free (argstr);
}

/* Enable pass specified by ARG.  */

void
enable_pass (const char *arg)
{
  enable_disable_pass (arg, true);
}

/* Disable pass specified by ARG.  */

void
disable_pass (const char *arg)
{
  enable_disable_pass (arg, false);
}

/* Returns true if PASS is explicitly enabled/disabled for FUNC.  */

static bool
is_pass_explicitly_enabled_or_disabled (opt_pass *pass,
					tree func,
					vec<uid_range_p> tab)
{
  uid_range_p slot, range;
  int cgraph_uid;
  const char *aname = NULL;

  if (!tab.exists ()
      || (unsigned) pass->static_pass_number >= tab.length ()
      || pass->static_pass_number == -1)
    return false;

  slot = tab[pass->static_pass_number];
  if (!slot)
    return false;

  cgraph_uid = func ? cgraph_node::get (func)->get_uid () : 0;
  if (func && DECL_ASSEMBLER_NAME_SET_P (func))
    aname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (func));

  range = slot;
  while (range)
    {
      if ((unsigned) cgraph_uid >= range->start
	  && (unsigned) cgraph_uid <= range->last)
	return true;
      if (range->assem_name && aname
          && !strcmp (range->assem_name, aname))
        return true;
      range = range->next;
    }

  return false;
}


/* Update static_pass_number for passes (and the flag
   TODO_mark_first_instance).

   Passes are constructed with static_pass_number preinitialized to 0

   This field is used in two different ways: initially as instance numbers
   of their kind, and then as ids within the entire pass manager.

   Within pass_manager::pass_manager:

   * In add_pass_instance(), as called by next_pass_1 in
     NEXT_PASS in init_optimization_passes

   * When the initial instance of a pass within a pass manager is seen,
     it is flagged, and its static_pass_number is set to -1

   * On subsequent times that it is seen, the static pass number
     is decremented each time, so that if there are e.g. 4 dups,
     they have static_pass_number -4, 2, 3, 4 respectively (note
     how the initial one is negative and gives the count); these
     can be thought of as instance numbers of the specific pass

   * Within the register_dump_files () traversal, set_pass_for_id()
     is called on each pass, using these instance numbers to create
     dumpfile switches, and then overwriting them with a pass id,
     which are global to the whole pass manager (based on
     (TDI_end + current value of extra_dump_files_in_use) )  */

static void
add_pass_instance (opt_pass *new_pass, bool track_duplicates,
		   opt_pass *initial_pass)
{
  /* Are we dealing with the first pass of its kind, or a clone?  */
  if (new_pass != initial_pass)
    {
      /* We're dealing with a clone.  */
      new_pass->todo_flags_start &= ~TODO_mark_first_instance;

      /* Indicate to register_dump_files that this pass has duplicates,
         and so it should rename the dump file.  The first instance will
         be -1, and be number of duplicates = -static_pass_number - 1.
         Subsequent instances will be > 0 and just the duplicate number.  */
      if ((new_pass->name && new_pass->name[0] != '*') || track_duplicates)
        {
          initial_pass->static_pass_number -= 1;
          new_pass->static_pass_number = -initial_pass->static_pass_number;
	}
    }
  else
    {
      /* We're dealing with the first pass of its kind.  */
      new_pass->todo_flags_start |= TODO_mark_first_instance;
      new_pass->static_pass_number = -1;

      invoke_plugin_callbacks (PLUGIN_NEW_PASS, new_pass);
    }
}

/* Add a pass to the pass list. Duplicate the pass if it's already
   in the list.  */

static opt_pass **
next_pass_1 (opt_pass **list, opt_pass *pass, opt_pass *initial_pass)
{
  /* Every pass should have a name so that plugins can refer to them.  */
  gcc_assert (pass->name != NULL);

  add_pass_instance (pass, false, initial_pass);
  *list = pass;

  return &(*list)->next;
}

/* List node for an inserted pass instance. We need to keep track of all
   the newly-added pass instances (with 'added_pass_nodes' defined below)
   so that we can register their dump files after pass-positioning is finished.
   Registering dumping files needs to be post-processed or the
   static_pass_number of the opt_pass object would be modified and mess up
   the dump file names of future pass instances to be added.  */

struct pass_list_node
{
  opt_pass *pass;
  struct pass_list_node *next;
};

static struct pass_list_node *added_pass_nodes = NULL;
static struct pass_list_node *prev_added_pass_node;

/* Insert the pass at the proper position. Return true if the pass
   is successfully added.

   NEW_PASS_INFO - new pass to be inserted
   PASS_LIST - root of the pass list to insert the new pass to  */

static bool
position_pass (struct register_pass_info *new_pass_info, opt_pass **pass_list)
{
  opt_pass *pass = *pass_list, *prev_pass = NULL;
  bool success = false;

  for ( ; pass; prev_pass = pass, pass = pass->next)
    {
      /* Check if the current pass is of the same type as the new pass and
         matches the name and the instance number of the reference pass.  */
      if (pass->type == new_pass_info->pass->type
          && pass->name
          && !strcmp (pass->name, new_pass_info->reference_pass_name)
          && ((new_pass_info->ref_pass_instance_number == 0)
              || (new_pass_info->ref_pass_instance_number ==
                  pass->static_pass_number)
              || (new_pass_info->ref_pass_instance_number == 1
                  && pass->todo_flags_start & TODO_mark_first_instance)))
        {
          opt_pass *new_pass;
          struct pass_list_node *new_pass_node;

	  if (new_pass_info->ref_pass_instance_number == 0)
	    {
	      new_pass = new_pass_info->pass->clone ();
	      add_pass_instance (new_pass, true, new_pass_info->pass);
	    }
	  else
	    {
	      new_pass = new_pass_info->pass;
	      add_pass_instance (new_pass, true, new_pass);
	    }

          /* Insert the new pass instance based on the positioning op.  */
          switch (new_pass_info->pos_op)
            {
              case PASS_POS_INSERT_AFTER:
                new_pass->next = pass->next;
                pass->next = new_pass;

		/* Skip newly inserted pass to avoid repeated
		   insertions in the case where the new pass and the
		   existing one have the same name.  */
                pass = new_pass;
                break;
              case PASS_POS_INSERT_BEFORE:
                new_pass->next = pass;
                if (prev_pass)
                  prev_pass->next = new_pass;
                else
                  *pass_list = new_pass;
                break;
              case PASS_POS_REPLACE:
                new_pass->next = pass->next;
                if (prev_pass)
                  prev_pass->next = new_pass;
                else
                  *pass_list = new_pass;
                new_pass->sub = pass->sub;
                new_pass->tv_id = pass->tv_id;
                pass = new_pass;
                break;
              default:
                error ("invalid pass positioning operation");
                return false;
            }

          /* Save the newly added pass (instance) in the added_pass_nodes
             list so that we can register its dump file later. Note that
             we cannot register the dump file now because doing so will modify
             the static_pass_number of the opt_pass object and therefore
             mess up the dump file name of future instances.  */
          new_pass_node = XCNEW (struct pass_list_node);
          new_pass_node->pass = new_pass;
          if (!added_pass_nodes)
            added_pass_nodes = new_pass_node;
          else
            prev_added_pass_node->next = new_pass_node;
          prev_added_pass_node = new_pass_node;

          success = true;
        }

      if (pass->sub && position_pass (new_pass_info, &pass->sub))
        success = true;
    }

  return success;
}

/* Hooks a new pass into the pass lists.

   PASS_INFO   - pass information that specifies the opt_pass object,
                 reference pass, instance number, and how to position
                 the pass  */

void
register_pass (struct register_pass_info *pass_info)
{
  g->get_passes ()->register_pass (pass_info);
}

void
register_pass (opt_pass* pass, pass_positioning_ops pos,
	       const char* ref_pass_name, int ref_pass_inst_number)
{
  register_pass_info i;
  i.pass = pass;
  i.reference_pass_name = ref_pass_name;
  i.ref_pass_instance_number = ref_pass_inst_number;
  i.pos_op = pos;

  g->get_passes ()->register_pass (&i);
}

void
pass_manager::register_pass (struct register_pass_info *pass_info)
{
  bool all_instances, success;
  gcc::dump_manager *dumps = m_ctxt->get_dumps ();

  /* The checks below could fail in buggy plugins.  Existing GCC
     passes should never fail these checks, so we mention plugin in
     the messages.  */
  if (!pass_info->pass)
      fatal_error (input_location, "plugin cannot register a missing pass");

  if (!pass_info->pass->name)
      fatal_error (input_location, "plugin cannot register an unnamed pass");

  if (!pass_info->reference_pass_name)
      fatal_error
	(input_location,
	 "plugin cannot register pass %qs without reference pass name",
	 pass_info->pass->name);

  /* Try to insert the new pass to the pass lists.  We need to check
     all five lists as the reference pass could be in one (or all) of
     them.  */
  all_instances = pass_info->ref_pass_instance_number == 0;
  success = position_pass (pass_info, &all_lowering_passes);
  if (!success || all_instances)
    success |= position_pass (pass_info, &all_small_ipa_passes);
  if (!success || all_instances)
    success |= position_pass (pass_info, &all_regular_ipa_passes);
  if (!success || all_instances)
    success |= position_pass (pass_info, &all_late_ipa_passes);
  if (!success || all_instances)
    success |= position_pass (pass_info, &all_passes);
  if (!success)
    fatal_error
      (input_location,
       "pass %qs not found but is referenced by new pass %qs",
       pass_info->reference_pass_name, pass_info->pass->name);

  /* OK, we have successfully inserted the new pass. We need to register
     the dump files for the newly added pass and its duplicates (if any).
     Because the registration of plugin/backend passes happens after the
     command-line options are parsed, the options that specify single
     pass dumping (e.g. -fdump-tree-PASSNAME) cannot be used for new
     passes. Therefore we currently can only enable dumping of
     new passes when the 'dump-all' flags (e.g. -fdump-tree-all)
     are specified. While doing so, we also delete the pass_list_node
     objects created during pass positioning.  */
  while (added_pass_nodes)
    {
      struct pass_list_node *next_node = added_pass_nodes->next;
      enum tree_dump_index tdi;
      register_one_dump_file (added_pass_nodes->pass);
      if (added_pass_nodes->pass->type == SIMPLE_IPA_PASS
          || added_pass_nodes->pass->type == IPA_PASS)
        tdi = TDI_ipa_all;
      else if (added_pass_nodes->pass->type == GIMPLE_PASS)
        tdi = TDI_tree_all;
      else
        tdi = TDI_rtl_all;
      /* Check if dump-all flag is specified.  */
      if (dumps->get_dump_file_info (tdi)->pstate)
	{
	  dumps->get_dump_file_info (added_pass_nodes->pass->static_pass_number)
            ->pstate = dumps->get_dump_file_info (tdi)->pstate;
	  dumps->get_dump_file_info (added_pass_nodes->pass->static_pass_number)
	    ->pflags = dumps->get_dump_file_info (tdi)->pflags;
	}
      XDELETE (added_pass_nodes);
      added_pass_nodes = next_node;
    }
}

/* Construct the pass tree.  The sequencing of passes is driven by
   the cgraph routines:

   finalize_compilation_unit ()
       for each node N in the cgraph
	   cgraph_analyze_function (N)
	       cgraph_lower_function (N) -> all_lowering_passes

   If we are optimizing, compile is then invoked:

   compile ()
       ipa_passes () 			-> all_small_ipa_passes
					-> Analysis of all_regular_ipa_passes
	* possible LTO streaming at copmilation time *
					-> Execution of all_regular_ipa_passes
	* possible LTO streaming at link time *
					-> all_late_ipa_passes
       expand_all_functions ()
           for each node N in the cgraph
	       expand_function (N)      -> Transformation of all_regular_ipa_passes
				        -> all_passes
*/

pass_manager::pass_manager (context *ctxt)
: all_passes (NULL), all_small_ipa_passes (NULL), all_lowering_passes (NULL),
  all_regular_ipa_passes (NULL),
  all_late_ipa_passes (NULL), passes_by_id (NULL), passes_by_id_size (0),
  m_ctxt (ctxt), m_name_to_pass_map (NULL)
{
  opt_pass **p;

  /* Zero-initialize pass members.  */
#define INSERT_PASSES_AFTER(PASS)
#define PUSH_INSERT_PASSES_WITHIN(PASS)
#define POP_INSERT_PASSES()
#define NEXT_PASS(PASS, NUM) PASS ## _ ## NUM = NULL
#define NEXT_PASS_WITH_ARG(PASS, NUM, ARG) NEXT_PASS (PASS, NUM)
#define TERMINATE_PASS_LIST(PASS)
#include "pass-instances.def"
#undef INSERT_PASSES_AFTER
#undef PUSH_INSERT_PASSES_WITHIN
#undef POP_INSERT_PASSES
#undef NEXT_PASS
#undef NEXT_PASS_WITH_ARG
#undef TERMINATE_PASS_LIST

  /* Initialize the pass_lists array.  */
#define DEF_PASS_LIST(LIST) pass_lists[PASS_LIST_NO_##LIST] = &LIST;
  GCC_PASS_LISTS
#undef DEF_PASS_LIST

  /* Build the tree of passes.  */

#define INSERT_PASSES_AFTER(PASS)		\
  {						\
    opt_pass **p_start;				\
    p_start = p = &(PASS);

#define TERMINATE_PASS_LIST(PASS)		\
    gcc_assert (p_start == &PASS);		\
    *p = NULL;					\
  }

#define PUSH_INSERT_PASSES_WITHIN(PASS) \
  { \
    opt_pass **p = &(PASS ## _1)->sub;

#define POP_INSERT_PASSES() \
  }

#define NEXT_PASS(PASS, NUM) \
  do { \
    gcc_assert (PASS ## _ ## NUM == NULL); \
    if ((NUM) == 1)                              \
      PASS ## _1 = make_##PASS (m_ctxt);          \
    else                                         \
      {                                          \
        gcc_assert (PASS ## _1);                 \
        PASS ## _ ## NUM = PASS ## _1->clone (); \
      }                                          \
    p = next_pass_1 (p, PASS ## _ ## NUM, PASS ## _1);  \
  } while (0)

#define NEXT_PASS_WITH_ARG(PASS, NUM, ARG)		\
    do {						\
      NEXT_PASS (PASS, NUM);				\
      PASS ## _ ## NUM->set_pass_param (0, ARG);	\
    } while (0)

#include "pass-instances.def"

#undef INSERT_PASSES_AFTER
#undef PUSH_INSERT_PASSES_WITHIN
#undef POP_INSERT_PASSES
#undef NEXT_PASS
#undef NEXT_PASS_WITH_ARG
#undef TERMINATE_PASS_LIST

  /* Register the passes with the tree dump code.  */
  register_dump_files (all_lowering_passes);
  register_dump_files (all_small_ipa_passes);
  register_dump_files (all_regular_ipa_passes);
  register_dump_files (all_late_ipa_passes);
  register_dump_files (all_passes);
}

static void
delete_pass_tree (opt_pass *pass)
{
  while (pass)
    {
      /* Recurse into child passes.  */
      delete_pass_tree (pass->sub);

      opt_pass *next = pass->next;

      /* Delete this pass.  */
      delete pass;

      /* Iterate onto sibling passes.  */
      pass = next;
    }
}

pass_manager::~pass_manager ()
{
  XDELETEVEC (passes_by_id);

  /* Call delete_pass_tree on each of the pass_lists.  */
#define DEF_PASS_LIST(LIST) \
    delete_pass_tree (*pass_lists[PASS_LIST_NO_##LIST]);
  GCC_PASS_LISTS
#undef DEF_PASS_LIST

}

/* If we are in IPA mode (i.e., current_function_decl is NULL), call
   function CALLBACK for every function in the call graph.  Otherwise,
   call CALLBACK on the current function.  */

static void
do_per_function (void (*callback) (function *, void *data), void *data)
{
  if (current_function_decl)
    callback (cfun, data);
  else
    {
      struct cgraph_node *node;
      FOR_EACH_DEFINED_FUNCTION (node)
	if (node->analyzed && (gimple_has_body_p (node->decl) && !in_lto_p)
	    && (!node->clone_of || node->decl != node->clone_of->decl))
	  callback (DECL_STRUCT_FUNCTION (node->decl), data);
    }
}

/* Because inlining might remove no-longer reachable nodes, we need to
   keep the array visible to garbage collector to avoid reading collected
   out nodes.  */
static int nnodes;
static GTY ((length ("nnodes"))) cgraph_node **order;

#define uid_hash_t hash_set<int_hash <int, 0, -1> >

/* Hook called when NODE is removed and therefore should be
   excluded from order vector.  DATA is a hash set with removed nodes.  */

static void
remove_cgraph_node_from_order (cgraph_node *node, void *data)
{
  uid_hash_t *removed_nodes = (uid_hash_t *)data;
  removed_nodes->add (node->get_uid ());
}

/* If we are in IPA mode (i.e., current_function_decl is NULL), call
   function CALLBACK for every function in the call graph.  Otherwise,
   call CALLBACK on the current function.
   This function is global so that plugins can use it.  */
void
do_per_function_toporder (void (*callback) (function *, void *data), void *data)
{
  int i;

  if (current_function_decl)
    callback (cfun, data);
  else
    {
      cgraph_node_hook_list *hook;
      uid_hash_t removed_nodes;
      gcc_assert (!order);
      order = ggc_vec_alloc<cgraph_node *> (symtab->cgraph_count);

      nnodes = ipa_reverse_postorder (order);
      for (i = nnodes - 1; i >= 0; i--)
	order[i]->process = 1;
      hook = symtab->add_cgraph_removal_hook (remove_cgraph_node_from_order,
					      &removed_nodes);
      for (i = nnodes - 1; i >= 0; i--)
	{
	  cgraph_node *node = order[i];

	  /* Function could be inlined and removed as unreachable.  */
	  if (node == NULL || removed_nodes.contains (node->get_uid ()))
	    continue;

	  /* Allow possibly removed nodes to be garbage collected.  */
	  order[i] = NULL;
	  node->process = 0;
	  if (node->has_gimple_body_p ())
	    {
	      struct function *fn = DECL_STRUCT_FUNCTION (node->decl);
	      push_cfun (fn);
	      callback (fn, data);
	      pop_cfun ();
	    }
	}
      symtab->remove_cgraph_removal_hook (hook);
    }
  ggc_free (order);
  order = NULL;
  nnodes = 0;
}

/* Helper function to perform function body dump.  */

static void
execute_function_dump (function *fn, void *data)
{
  opt_pass *pass = (opt_pass *)data;

  if (dump_file)
    {
      push_cfun (fn);

      if (fn->curr_properties & PROP_trees)
        dump_function_to_file (fn->decl, dump_file, dump_flags);
      else
	print_rtl_with_bb (dump_file, get_insns (), dump_flags);

      /* Flush the file.  If verification fails, we won't be able to
	 close the file before aborting.  */
      fflush (dump_file);

      if ((fn->curr_properties & PROP_cfg)
	  && (dump_flags & TDF_GRAPH))
	{
	  gcc::dump_manager *dumps = g->get_dumps ();
	  struct dump_file_info *dfi
	    = dumps->get_dump_file_info (pass->static_pass_number);
	  if (!dfi->graph_dump_initialized)
	    {
	      clean_graph_dump_file (dump_file_name);
	      dfi->graph_dump_initialized = true;
	    }
	  print_graph_cfg (dump_file_name, fn);
	}

      pop_cfun ();
    }
}

/* This function is called when an internal compiler error is encountered.
   Ensure that function dump is made available before compiler is aborted.  */

void
emergency_dump_function ()
{
  if (!current_pass)
    return;
  enum opt_pass_type pt = current_pass->type;
  fnotice (stderr, "during %s pass: %s\n",
	   pt == GIMPLE_PASS ? "GIMPLE" : pt == RTL_PASS ? "RTL" : "IPA",
	   current_pass->name);
  if (!dump_file || !cfun)
    return;
  fnotice (stderr, "dump file: %s\n", dump_file_name);
  fprintf (dump_file, "\n\n\nEMERGENCY DUMP:\n\n");
  execute_function_dump (cfun, current_pass);
}

static struct profile_record *profile_record;

/* Do profile consistency book-keeping for the pass with static number INDEX.
   If SUBPASS is zero, we run _before_ the pass, and if SUBPASS is one, then
   we run _after_ the pass.  RUN is true if the pass really runs, or FALSE
   if we are only book-keeping on passes that may have selectively disabled
   themselves on a given function.  */
static void
check_profile_consistency (int index, int subpass, bool run)
{
  pass_manager *passes = g->get_passes ();
  if (index == -1)
    return;
  if (!profile_record)
    profile_record = XCNEWVEC (struct profile_record,
			       passes->passes_by_id_size);
  gcc_assert (index < passes->passes_by_id_size && index >= 0);
  gcc_assert (subpass < 2);
  profile_record[index].run |= run;
  account_profile_record (&profile_record[index], subpass);
}

/* Output profile consistency.  */

void
dump_profile_report (void)
{
  g->get_passes ()->dump_profile_report ();
}

void
pass_manager::dump_profile_report () const
{
  int i, j;
  int last_freq_in = 0, last_count_in = 0, last_freq_out = 0, last_count_out = 0;
  gcov_type last_time = 0, last_size = 0;
  double rel_time_change, rel_size_change;
  int last_reported = 0;

  if (!profile_record)
    return;
  fprintf (stderr, "\nProfile consistency report:\n\n");
  fprintf (stderr, "Pass name                        |mismatch in |mismated out|Overall\n");
  fprintf (stderr, "                                 |freq count  |freq count  |size      time\n");
	   
  for (i = 0; i < passes_by_id_size; i++)
    for (j = 0 ; j < 2; j++)
      if (profile_record[i].run)
	{
	  if (last_time)
	    rel_time_change = (profile_record[i].time[j]
			       - (double)last_time) * 100 / (double)last_time;
	  else
	    rel_time_change = 0;
	  if (last_size)
	    rel_size_change = (profile_record[i].size[j]
			       - (double)last_size) * 100 / (double)last_size;
	  else
	    rel_size_change = 0;

	  if (profile_record[i].num_mismatched_freq_in[j] != last_freq_in
	      || profile_record[i].num_mismatched_freq_out[j] != last_freq_out
	      || profile_record[i].num_mismatched_count_in[j] != last_count_in
	      || profile_record[i].num_mismatched_count_out[j] != last_count_out
	      || rel_time_change || rel_size_change)
	    {
	      last_reported = i;
              fprintf (stderr, "%-20s %s",
		       passes_by_id [i]->name,
		       j ? "(after TODO)" : "            ");
	      if (profile_record[i].num_mismatched_freq_in[j] != last_freq_in)
		fprintf (stderr, "| %+5i",
		         profile_record[i].num_mismatched_freq_in[j]
			  - last_freq_in);
	      else
		fprintf (stderr, "|      ");
	      if (profile_record[i].num_mismatched_count_in[j] != last_count_in)
		fprintf (stderr, " %+5i",
		         profile_record[i].num_mismatched_count_in[j]
			  - last_count_in);
	      else
		fprintf (stderr, "      ");
	      if (profile_record[i].num_mismatched_freq_out[j] != last_freq_out)
		fprintf (stderr, "| %+5i",
		         profile_record[i].num_mismatched_freq_out[j]
			  - last_freq_out);
	      else
		fprintf (stderr, "|      ");
	      if (profile_record[i].num_mismatched_count_out[j] != last_count_out)
		fprintf (stderr, " %+5i",
		         profile_record[i].num_mismatched_count_out[j]
			  - last_count_out);
	      else
		fprintf (stderr, "      ");

	      /* Size/time units change across gimple and RTL.  */
	      if (i == pass_expand_1->static_pass_number)
		fprintf (stderr, "|----------");
	      else
		{
		  if (rel_size_change)
		    fprintf (stderr, "| %+8.4f%%", rel_size_change);
		  else
		    fprintf (stderr, "|          ");
		  if (rel_time_change)
		    fprintf (stderr, " %+8.4f%%", rel_time_change);
		}
	      fprintf (stderr, "\n");
	      last_freq_in = profile_record[i].num_mismatched_freq_in[j];
	      last_freq_out = profile_record[i].num_mismatched_freq_out[j];
	      last_count_in = profile_record[i].num_mismatched_count_in[j];
	      last_count_out = profile_record[i].num_mismatched_count_out[j];
	    }
	  else if (j && last_reported != i)
	    {
	      last_reported = i;
              fprintf (stderr, "%-20s ------------|            |            |\n",
		       passes_by_id [i]->name);
	    }
	  last_time = profile_record[i].time[j];
	  last_size = profile_record[i].size[j];
	}
}

/* Perform all TODO actions that ought to be done on each function.  */

static void
execute_function_todo (function *fn, void *data)
{
  bool from_ipa_pass = (cfun == NULL);
  unsigned int flags = (size_t)data;
  flags &= ~fn->last_verified;
  if (!flags)
    return;

  push_cfun (fn);

  /* Always cleanup the CFG before trying to update SSA.  */
  if (flags & TODO_cleanup_cfg)
    {
      cleanup_tree_cfg ();

      /* When cleanup_tree_cfg merges consecutive blocks, it may
	 perform some simplistic propagation when removing single
	 valued PHI nodes.  This propagation may, in turn, cause the
	 SSA form to become out-of-date (see PR 22037).  So, even
	 if the parent pass had not scheduled an SSA update, we may
	 still need to do one.  */
      if (!(flags & TODO_update_ssa_any) && need_ssa_update_p (cfun))
	flags |= TODO_update_ssa;
    }

  if (flags & TODO_update_ssa_any)
    {
      unsigned update_flags = flags & TODO_update_ssa_any;
      update_ssa (update_flags);
    }

  if (flag_tree_pta && (flags & TODO_rebuild_alias))
    compute_may_aliases ();

  if (optimize && (flags & TODO_update_address_taken))
    execute_update_addresses_taken ();

  if (flags & TODO_remove_unused_locals)
    remove_unused_locals ();

  if (flags & TODO_rebuild_frequencies)
    rebuild_frequencies ();

  if (flags & TODO_rebuild_cgraph_edges)
    cgraph_edge::rebuild_edges ();

  gcc_assert (dom_info_state (fn, CDI_POST_DOMINATORS) == DOM_NONE);
  /* If we've seen errors do not bother running any verifiers.  */
  if (flag_checking && !seen_error ())
    {
      dom_state pre_verify_state = dom_info_state (fn, CDI_DOMINATORS);
      dom_state pre_verify_pstate = dom_info_state (fn, CDI_POST_DOMINATORS);

      if (flags & TODO_verify_il)
	{
	  if (cfun->curr_properties & PROP_trees)
	    {
	      if (cfun->curr_properties & PROP_cfg)
		/* IPA passes leave stmts to be fixed up, so make sure to
		   not verify stmts really throw.  */
		verify_gimple_in_cfg (cfun, !from_ipa_pass);
	      else
		verify_gimple_in_seq (gimple_body (cfun->decl));
	    }
	  if (cfun->curr_properties & PROP_ssa)
	    /* IPA passes leave stmts to be fixed up, so make sure to
	       not verify SSA operands whose verifier will choke on that.  */
	    verify_ssa (true, !from_ipa_pass);
	  /* IPA passes leave basic-blocks unsplit, so make sure to
	     not trip on that.  */
	  if ((cfun->curr_properties & PROP_cfg)
	      && !from_ipa_pass)
	    verify_flow_info ();
	  if (current_loops
	      && ! loops_state_satisfies_p (LOOPS_NEED_FIXUP))
	    {
	      verify_loop_structure ();
	      if (loops_state_satisfies_p (LOOP_CLOSED_SSA))
		verify_loop_closed_ssa (false);
	    }
	  if (cfun->curr_properties & PROP_rtl)
	    verify_rtl_sharing ();
	}

      /* Make sure verifiers don't change dominator state.  */
      gcc_assert (dom_info_state (fn, CDI_DOMINATORS) == pre_verify_state);
      gcc_assert (dom_info_state (fn, CDI_POST_DOMINATORS) == pre_verify_pstate);
    }

  fn->last_verified = flags & TODO_verify_all;

  pop_cfun ();

  /* For IPA passes make sure to release dominator info, it can be
     computed by non-verifying TODOs.  */
  if (from_ipa_pass)
    {
      free_dominance_info (fn, CDI_DOMINATORS);
      free_dominance_info (fn, CDI_POST_DOMINATORS);
    }
}

/* Perform all TODO actions.  */
static void
execute_todo (unsigned int flags)
{
  if (flag_checking
      && cfun
      && need_ssa_update_p (cfun))
    gcc_assert (flags & TODO_update_ssa_any);

  statistics_fini_pass ();

  if (flags)
    do_per_function (execute_function_todo, (void *)(size_t) flags);

  /* At this point we should not have any unreachable code in the
     CFG, so it is safe to flush the pending freelist for SSA_NAMES.  */
  if (cfun && cfun->gimple_df)
    flush_ssaname_freelist ();

  /* Always remove functions just as before inlining: IPA passes might be
     interested to see bodies of extern inline functions that are not inlined
     to analyze side effects.  The full removal is done just at the end
     of IPA pass queue.  */
  if (flags & TODO_remove_functions)
    {
      gcc_assert (!cfun);
      symtab->remove_unreachable_nodes (dump_file);
    }

  if ((flags & TODO_dump_symtab) && dump_file && !current_function_decl)
    {
      gcc_assert (!cfun);
      symtab->dump (dump_file);
      /* Flush the file.  If verification fails, we won't be able to
	 close the file before aborting.  */
      fflush (dump_file);
    }

  /* Now that the dumping has been done, we can get rid of the optional
     df problems.  */
  if (flags & TODO_df_finish)
    df_finish_pass ((flags & TODO_df_verify) != 0);
}

/* Verify invariants that should hold between passes.  This is a place
   to put simple sanity checks.  */

static void
verify_interpass_invariants (void)
{
  gcc_checking_assert (!fold_deferring_overflow_warnings_p ());
}

/* Clear the last verified flag.  */

static void
clear_last_verified (function *fn, void *data ATTRIBUTE_UNUSED)
{
  fn->last_verified = 0;
}

/* Helper function. Verify that the properties has been turn into the
   properties expected by the pass.  */

static void
verify_curr_properties (function *fn, void *data)
{
  unsigned int props = (size_t)data;
  gcc_assert ((fn->curr_properties & props) == props);
}

/* Release dump file name if set.  */

static void
release_dump_file_name (void)
{
  if (dump_file_name)
    {
      free (CONST_CAST (char *, dump_file_name));
      dump_file_name = NULL;
    }
}

/* Initialize pass dump file.  */
/* This is non-static so that the plugins can use it.  */

bool
pass_init_dump_file (opt_pass *pass)
{
  /* If a dump file name is present, open it if enabled.  */
  if (pass->static_pass_number != -1)
    {
      timevar_push (TV_DUMP);
      gcc::dump_manager *dumps = g->get_dumps ();
      bool initializing_dump =
	!dumps->dump_initialized_p (pass->static_pass_number);
      release_dump_file_name ();
      dump_file_name = dumps->get_dump_file_name (pass->static_pass_number);
      dumps->dump_start (pass->static_pass_number, &dump_flags);
      if (dump_file && current_function_decl && ! (dump_flags & TDF_GIMPLE))
        dump_function_header (dump_file, current_function_decl, dump_flags);
      if (initializing_dump
	  && dump_file && (dump_flags & TDF_GRAPH)
	  && cfun && (cfun->curr_properties & PROP_cfg))
	{
	  clean_graph_dump_file (dump_file_name);
	  struct dump_file_info *dfi
	    = dumps->get_dump_file_info (pass->static_pass_number);
	  dfi->graph_dump_initialized = true;
	}
      timevar_pop (TV_DUMP);
      return initializing_dump;
    }
  else
    return false;
}

/* Flush PASS dump file.  */
/* This is non-static so that plugins can use it.  */

void
pass_fini_dump_file (opt_pass *pass)
{
  timevar_push (TV_DUMP);

  /* Flush and close dump file.  */
  release_dump_file_name ();

  g->get_dumps ()->dump_finish (pass->static_pass_number);
  timevar_pop (TV_DUMP);
}

/* After executing the pass, apply expected changes to the function
   properties. */

static void
update_properties_after_pass (function *fn, void *data)
{
  opt_pass *pass = (opt_pass *) data;
  fn->curr_properties = (fn->curr_properties | pass->properties_provided)
		         & ~pass->properties_destroyed;
}

/* Execute summary generation for all of the passes in IPA_PASS.  */

void
execute_ipa_summary_passes (ipa_opt_pass_d *ipa_pass)
{
  while (ipa_pass)
    {
      opt_pass *pass = ipa_pass;

      /* Execute all of the IPA_PASSes in the list.  */
      if (ipa_pass->type == IPA_PASS
	  && pass->gate (cfun)
	  && ipa_pass->generate_summary)
	{
	  pass_init_dump_file (pass);

	  /* If a timevar is present, start it.  */
	  if (pass->tv_id)
	    timevar_push (pass->tv_id);

	  current_pass = pass;
	  ipa_pass->generate_summary ();

	  /* Stop timevar.  */
	  if (pass->tv_id)
	    timevar_pop (pass->tv_id);

	  pass_fini_dump_file (pass);
	}
      ipa_pass = (ipa_opt_pass_d *)ipa_pass->next;
    }
}

/* Execute IPA_PASS function transform on NODE.  */

static void
execute_one_ipa_transform_pass (struct cgraph_node *node,
				ipa_opt_pass_d *ipa_pass)
{
  opt_pass *pass = ipa_pass;
  unsigned int todo_after = 0;

  current_pass = pass;
  if (!ipa_pass->function_transform)
    return;

  /* Note that the folders should only create gimple expressions.
     This is a hack until the new folder is ready.  */
  in_gimple_form = (cfun && (cfun->curr_properties & PROP_trees)) != 0;

  pass_init_dump_file (pass);

  /* If a timevar is present, start it.  */
  if (pass->tv_id != TV_NONE)
    timevar_push (pass->tv_id);

  /* Run pre-pass verification.  */
  execute_todo (ipa_pass->function_transform_todo_flags_start);

  /* Do it!  */
  todo_after = ipa_pass->function_transform (node);

  if (profile_report && cfun && (cfun->curr_properties & PROP_cfg))
    check_profile_consistency (pass->static_pass_number, 0, true);

  /* Run post-pass cleanup and verification.  */
  execute_todo (todo_after);
  verify_interpass_invariants ();
  if (profile_report && cfun && (cfun->curr_properties & PROP_cfg))
    check_profile_consistency (pass->static_pass_number, 1, true);

  /* Stop timevar.  */
  if (pass->tv_id != TV_NONE)
    timevar_pop (pass->tv_id);

  if (dump_file)
    do_per_function (execute_function_dump, pass);
  pass_fini_dump_file (pass);

  current_pass = NULL;
  redirect_edge_var_map_empty ();

  /* Signal this is a suitable GC collection point.  */
  if (!(todo_after & TODO_do_not_ggc_collect))
    ggc_collect ();
}

/* For the current function, execute all ipa transforms. */

void
execute_all_ipa_transforms (void)
{
  struct cgraph_node *node;
  if (!cfun)
    return;
  node = cgraph_node::get (current_function_decl);

  if (node->ipa_transforms_to_apply.exists ())
    {
      unsigned int i;

      for (i = 0; i < node->ipa_transforms_to_apply.length (); i++)
	execute_one_ipa_transform_pass (node, node->ipa_transforms_to_apply[i]);
      node->ipa_transforms_to_apply.release ();
    }
}

/* Check if PASS is explicitly disabled or enabled and return
   the gate status.  FUNC is the function to be processed, and
   GATE_STATUS is the gate status determined by pass manager by
   default.  */

static bool
override_gate_status (opt_pass *pass, tree func, bool gate_status)
{
  bool explicitly_enabled = false;
  bool explicitly_disabled = false;

  explicitly_enabled
   = is_pass_explicitly_enabled_or_disabled (pass, func,
                                             enabled_pass_uid_range_tab);
  explicitly_disabled
   = is_pass_explicitly_enabled_or_disabled (pass, func,
                                             disabled_pass_uid_range_tab);

  gate_status = !explicitly_disabled && (gate_status || explicitly_enabled);

  return gate_status;
}

/* Determine if PASS_NAME matches CRITERION.
   Not a pure predicate, since it can update CRITERION, to support
   matching the Nth invocation of a pass.
   Subroutine of should_skip_pass_p.  */

static bool
determine_pass_name_match (const char *pass_name, char *criterion)
{
  size_t namelen = strlen (pass_name);
  if (! strncmp (pass_name, criterion, namelen))
    {
      /* The following supports starting with the Nth invocation
	 of a pass (where N does not necessarily is equal to the
	 dump file suffix).  */
      if (criterion[namelen] == '\0'
	  || (criterion[namelen] == '1'
	      && criterion[namelen + 1] == '\0'))
	return true;
      else
	{
	  if (criterion[namelen + 1] == '\0')
	    --criterion[namelen];
	  return false;
	}
    }
  else
    return false;
}

/* For skipping passes until "startwith" pass.
   Return true iff PASS should be skipped.
   Clear cfun->pass_startwith when encountering the "startwith" pass,
   so that all subsequent passes are run.  */

static bool
should_skip_pass_p (opt_pass *pass)
{
  if (!cfun)
    return false;
  if (!cfun->pass_startwith)
    return false;

  /* For __GIMPLE functions, we have to at least start when we leave
     SSA.  Hence, we need to detect the "expand" pass, and stop skipping
     when we encounter it.  A cheap way to identify "expand" is it to
     detect the destruction of PROP_ssa.
     For __RTL functions, we invoke "rest_of_compilation" directly, which
     is after "expand", and hence we don't reach this conditional.  */
  if (pass->properties_destroyed & PROP_ssa)
    {
      if (!quiet_flag)
	fprintf (stderr, "starting anyway when leaving SSA: %s\n", pass->name);
      cfun->pass_startwith = NULL;
      return false;
    }

  if (determine_pass_name_match (pass->name, cfun->pass_startwith))
    {
      if (!quiet_flag)
	fprintf (stderr, "found starting pass: %s\n", pass->name);
      cfun->pass_startwith = NULL;
      return false;
    }

  /* For GIMPLE passes, run any property provider (but continue skipping
     afterwards).
     We don't want to force running RTL passes that are property providers:
     "expand" is covered above, and the only pass other than "expand" that
     provides a property is "into_cfglayout" (PROP_cfglayout), which does
     too much for a dumped __RTL function.  */
  if (pass->type == GIMPLE_PASS
      && pass->properties_provided != 0)
    return false;

  /* Don't skip df init; later RTL passes need it.  */
  if (strstr (pass->name, "dfinit") != NULL)
    return false;

  if (!quiet_flag)
    fprintf (stderr, "skipping pass: %s\n", pass->name);

  /* If we get here, then we have a "startwith" that we haven't seen yet;
     skip the pass.  */
  return true;
}

/* Skip the given pass, for handling passes before "startwith"
   in __GIMPLE and__RTL-marked functions.
   In theory, this ought to be a no-op, but some of the RTL passes
   need additional processing here.  */

static void
skip_pass (opt_pass *pass)
{
  /* Pass "reload" sets the global "reload_completed", and many
     things depend on this (e.g. instructions in .md files).  */
  if (strcmp (pass->name, "reload") == 0)
    reload_completed = 1;

  /* The INSN_ADDRESSES vec is normally set up by
     shorten_branches; set it up for the benefit of passes that
     run after this.  */
  if (strcmp (pass->name, "shorten") == 0)
    INSN_ADDRESSES_ALLOC (get_max_uid ());

  /* Update the cfg hooks as appropriate.  */
  if (strcmp (pass->name, "into_cfglayout") == 0)
    {
      cfg_layout_rtl_register_cfg_hooks ();
      cfun->curr_properties |= PROP_cfglayout;
    }
  if (strcmp (pass->name, "outof_cfglayout") == 0)
    {
      rtl_register_cfg_hooks ();
      cfun->curr_properties &= ~PROP_cfglayout;
    }
}

/* Execute PASS. */

bool
execute_one_pass (opt_pass *pass)
{
  unsigned int todo_after = 0;

  bool gate_status;

  /* IPA passes are executed on whole program, so cfun should be NULL.
     Other passes need function context set.  */
  if (pass->type == SIMPLE_IPA_PASS || pass->type == IPA_PASS)
    gcc_assert (!cfun && !current_function_decl);
  else
    gcc_assert (cfun && current_function_decl);

  current_pass = pass;

  /* Check whether gate check should be avoided.
     User controls the value of the gate through the parameter "gate_status". */
  gate_status = pass->gate (cfun);
  gate_status = override_gate_status (pass, current_function_decl, gate_status);

  /* Override gate with plugin.  */
  invoke_plugin_callbacks (PLUGIN_OVERRIDE_GATE, &gate_status);

  if (!gate_status)
    {
      /* Run so passes selectively disabling themselves on a given function
	 are not miscounted.  */
      if (profile_report && cfun && (cfun->curr_properties & PROP_cfg))
	{
          check_profile_consistency (pass->static_pass_number, 0, false);
          check_profile_consistency (pass->static_pass_number, 1, false);
	}
      current_pass = NULL;
      return false;
    }

  if (should_skip_pass_p (pass))
    {
      skip_pass (pass);
      return true;
    }

  /* Pass execution event trigger: useful to identify passes being
     executed.  */
  invoke_plugin_callbacks (PLUGIN_PASS_EXECUTION, pass);

  if (!quiet_flag && !cfun)
    fprintf (stderr, " <%s>", pass->name ? pass->name : "");

  /* Note that the folders should only create gimple expressions.
     This is a hack until the new folder is ready.  */
  in_gimple_form = (cfun && (cfun->curr_properties & PROP_trees)) != 0;

  pass_init_dump_file (pass);

  /* If a timevar is present, start it.  */
  if (pass->tv_id != TV_NONE)
    timevar_push (pass->tv_id);

  /* Run pre-pass verification.  */
  execute_todo (pass->todo_flags_start);

  if (flag_checking)
    do_per_function (verify_curr_properties,
		     (void *)(size_t)pass->properties_required);

  /* Do it!  */
  todo_after = pass->execute (cfun);

  if (todo_after & TODO_discard_function)
    {
      /* Stop timevar.  */
      if (pass->tv_id != TV_NONE)
	timevar_pop (pass->tv_id);

      pass_fini_dump_file (pass);

      gcc_assert (cfun);
      /* As cgraph_node::release_body expects release dominators info,
	 we have to release it.  */
      if (dom_info_available_p (CDI_DOMINATORS))
       free_dominance_info (CDI_DOMINATORS);

      if (dom_info_available_p (CDI_POST_DOMINATORS))
       free_dominance_info (CDI_POST_DOMINATORS);

      tree fn = cfun->decl;
      pop_cfun ();
      gcc_assert (!cfun);
      cgraph_node::get (fn)->release_body ();

      current_pass = NULL;
      redirect_edge_var_map_empty ();

      ggc_collect ();

      return true;
    }

  do_per_function (clear_last_verified, NULL);

  do_per_function (update_properties_after_pass, pass);

  if (profile_report && cfun && (cfun->curr_properties & PROP_cfg))
    check_profile_consistency (pass->static_pass_number, 0, true);

  /* Run post-pass cleanup and verification.  */
  execute_todo (todo_after | pass->todo_flags_finish | TODO_verify_il);
  if (profile_report && cfun && (cfun->curr_properties & PROP_cfg))
    check_profile_consistency (pass->static_pass_number, 1, true);

  verify_interpass_invariants ();

  /* Stop timevar.  */
  if (pass->tv_id != TV_NONE)
    timevar_pop (pass->tv_id);

  if (pass->type == IPA_PASS
      && ((ipa_opt_pass_d *)pass)->function_transform)
    {
      struct cgraph_node *node;
      FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
	node->ipa_transforms_to_apply.safe_push ((ipa_opt_pass_d *)pass);
    }
  else if (dump_file)
    do_per_function (execute_function_dump, pass);

  if (!current_function_decl)
    symtab->process_new_functions ();

  pass_fini_dump_file (pass);

  if (pass->type != SIMPLE_IPA_PASS && pass->type != IPA_PASS)
    gcc_assert (!(cfun->curr_properties & PROP_trees)
		|| pass->type != RTL_PASS);

  current_pass = NULL;
  redirect_edge_var_map_empty ();

  /* Signal this is a suitable GC collection point.  */
  if (!((todo_after | pass->todo_flags_finish) & TODO_do_not_ggc_collect))
    ggc_collect ();

  return true;
}

static void
execute_pass_list_1 (opt_pass *pass)
{
  do
    {
      gcc_assert (pass->type == GIMPLE_PASS
		  || pass->type == RTL_PASS);

      if (cfun == NULL)
	return;
      if (execute_one_pass (pass) && pass->sub)
	execute_pass_list_1 (pass->sub);
      pass = pass->next;
    }
  while (pass);
}

void
execute_pass_list (function *fn, opt_pass *pass)
{
  gcc_assert (fn == cfun);
  execute_pass_list_1 (pass);
  if (cfun && fn->cfg)
    {
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
    }
}

/* Write out all LTO data.  */
static void
write_lto (void)
{
  timevar_push (TV_IPA_LTO_GIMPLE_OUT);
  lto_output ();
  timevar_pop (TV_IPA_LTO_GIMPLE_OUT);
  timevar_push (TV_IPA_LTO_DECL_OUT);
  produce_asm_for_decls ();
  timevar_pop (TV_IPA_LTO_DECL_OUT);
}

/* Same as execute_pass_list but assume that subpasses of IPA passes
   are local passes. If SET is not NULL, write out summaries of only
   those node in SET. */

static void
ipa_write_summaries_2 (opt_pass *pass, struct lto_out_decl_state *state)
{
  while (pass)
    {
      ipa_opt_pass_d *ipa_pass = (ipa_opt_pass_d *)pass;
      gcc_assert (!current_function_decl);
      gcc_assert (!cfun);
      gcc_assert (pass->type == SIMPLE_IPA_PASS || pass->type == IPA_PASS);
      if (pass->type == IPA_PASS
	  && ipa_pass->write_summary
	  && pass->gate (cfun))
	{
	  /* If a timevar is present, start it.  */
	  if (pass->tv_id)
	    timevar_push (pass->tv_id);

          pass_init_dump_file (pass);

	  current_pass = pass;
	  ipa_pass->write_summary ();

          pass_fini_dump_file (pass);

	  /* If a timevar is present, start it.  */
	  if (pass->tv_id)
	    timevar_pop (pass->tv_id);
	}

      if (pass->sub && pass->sub->type != GIMPLE_PASS)
	ipa_write_summaries_2 (pass->sub, state);

      pass = pass->next;
    }
}

/* Helper function of ipa_write_summaries. Creates and destroys the
   decl state and calls ipa_write_summaries_2 for all passes that have
   summaries.  SET is the set of nodes to be written.  */

static void
ipa_write_summaries_1 (lto_symtab_encoder_t encoder)
{
  pass_manager *passes = g->get_passes ();
  struct lto_out_decl_state *state = lto_new_out_decl_state ();
  state->symtab_node_encoder = encoder;

  lto_output_init_mode_table ();
  lto_push_out_decl_state (state);

  gcc_assert (!flag_wpa);
  ipa_write_summaries_2 (passes->all_regular_ipa_passes, state);

  write_lto ();

  gcc_assert (lto_get_out_decl_state () == state);
  lto_pop_out_decl_state ();
  lto_delete_out_decl_state (state);
}

/* Write out summaries for all the nodes in the callgraph.  */

void
ipa_write_summaries (void)
{
  lto_symtab_encoder_t encoder;
  int i, order_pos;
  varpool_node *vnode;
  struct cgraph_node *node;
  struct cgraph_node **order;

  if ((!flag_generate_lto && !flag_generate_offload) || seen_error ())
    return;

  gcc_assert (!dump_file);
  streamer_dump_file = dump_begin (TDI_lto_stream_out, NULL);

  select_what_to_stream ();

  encoder = lto_symtab_encoder_new (false);

  /* Create the callgraph set in the same order used in
     cgraph_expand_all_functions.  This mostly facilitates debugging,
     since it causes the gimple file to be processed in the same order
     as the source code.  */
  order = XCNEWVEC (struct cgraph_node *, symtab->cgraph_count);
  order_pos = ipa_reverse_postorder (order);
  gcc_assert (order_pos == symtab->cgraph_count);

  for (i = order_pos - 1; i >= 0; i--)
    {
      struct cgraph_node *node = order[i];

      if (gimple_has_body_p (node->decl))
	{
	  /* When streaming out references to statements as part of some IPA
	     pass summary, the statements need to have uids assigned and the
	     following does that for all the IPA passes here. Naturally, this
	     ordering then matches the one IPA-passes get in their stmt_fixup
	     hooks.  */

	  push_cfun (DECL_STRUCT_FUNCTION (node->decl));
	  renumber_gimple_stmt_uids ();
	  pop_cfun ();
	}
      if (node->definition && node->need_lto_streaming)
        lto_set_symtab_encoder_in_partition (encoder, node);
    }

  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->alias && node->need_lto_streaming)
      lto_set_symtab_encoder_in_partition (encoder, node);
  FOR_EACH_DEFINED_VARIABLE (vnode)
    if (vnode->need_lto_streaming)
      lto_set_symtab_encoder_in_partition (encoder, vnode);

  ipa_write_summaries_1 (compute_ltrans_boundary (encoder));

  free (order);
  if (streamer_dump_file)
    {
      dump_end (TDI_lto_stream_out, streamer_dump_file);
      streamer_dump_file = NULL;
    }
}

/* Same as execute_pass_list but assume that subpasses of IPA passes
   are local passes. If SET is not NULL, write out optimization summaries of
   only those node in SET. */

static void
ipa_write_optimization_summaries_1 (opt_pass *pass,
				    struct lto_out_decl_state *state)
{
  while (pass)
    {
      ipa_opt_pass_d *ipa_pass = (ipa_opt_pass_d *)pass;
      gcc_assert (!current_function_decl);
      gcc_assert (!cfun);
      gcc_assert (pass->type == SIMPLE_IPA_PASS || pass->type == IPA_PASS);
      if (pass->type == IPA_PASS
	  && ipa_pass->write_optimization_summary
	  && pass->gate (cfun))
	{
	  /* If a timevar is present, start it.  */
	  if (pass->tv_id)
	    timevar_push (pass->tv_id);

          pass_init_dump_file (pass);

	  current_pass = pass;
	  ipa_pass->write_optimization_summary ();

          pass_fini_dump_file (pass);

	  /* If a timevar is present, start it.  */
	  if (pass->tv_id)
	    timevar_pop (pass->tv_id);
	}

      if (pass->sub && pass->sub->type != GIMPLE_PASS)
	ipa_write_optimization_summaries_1 (pass->sub, state);

      pass = pass->next;
    }
}

/* Write all the optimization summaries for the cgraph nodes in SET.  If SET is
   NULL, write out all summaries of all nodes. */

void
ipa_write_optimization_summaries (lto_symtab_encoder_t encoder)
{
  struct lto_out_decl_state *state = lto_new_out_decl_state ();
  lto_symtab_encoder_iterator lsei;
  state->symtab_node_encoder = encoder;

  lto_output_init_mode_table ();
  lto_push_out_decl_state (state);
  for (lsei = lsei_start_function_in_partition (encoder);
       !lsei_end_p (lsei); lsei_next_function_in_partition (&lsei))
    {
      struct cgraph_node *node = lsei_cgraph_node (lsei);
      /* When streaming out references to statements as part of some IPA
	 pass summary, the statements need to have uids assigned.

	 For functions newly born at WPA stage we need to initialize
	 the uids here.  */
      if (node->definition
	  && gimple_has_body_p (node->decl))
	{
	  push_cfun (DECL_STRUCT_FUNCTION (node->decl));
	  renumber_gimple_stmt_uids ();
	  pop_cfun ();
	}
    }

  gcc_assert (flag_wpa);
  pass_manager *passes = g->get_passes ();
  ipa_write_optimization_summaries_1 (passes->all_regular_ipa_passes, state);

  write_lto ();

  gcc_assert (lto_get_out_decl_state () == state);
  lto_pop_out_decl_state ();
  lto_delete_out_decl_state (state);
}

/* Same as execute_pass_list but assume that subpasses of IPA passes
   are local passes.  */

static void
ipa_read_summaries_1 (opt_pass *pass)
{
  while (pass)
    {
      ipa_opt_pass_d *ipa_pass = (ipa_opt_pass_d *) pass;

      gcc_assert (!current_function_decl);
      gcc_assert (!cfun);
      gcc_assert (pass->type == SIMPLE_IPA_PASS || pass->type == IPA_PASS);

      if (pass->gate (cfun))
	{
	  if (pass->type == IPA_PASS && ipa_pass->read_summary)
	    {
	      /* If a timevar is present, start it.  */
	      if (pass->tv_id)
		timevar_push (pass->tv_id);

	      pass_init_dump_file (pass);

	      current_pass = pass;
	      ipa_pass->read_summary ();

	      pass_fini_dump_file (pass);

	      /* Stop timevar.  */
	      if (pass->tv_id)
		timevar_pop (pass->tv_id);
	    }

	  if (pass->sub && pass->sub->type != GIMPLE_PASS)
	    ipa_read_summaries_1 (pass->sub);
	}
      pass = pass->next;
    }
}


/* Read all the summaries for all_regular_ipa_passes.  */

void
ipa_read_summaries (void)
{
  pass_manager *passes = g->get_passes ();
  ipa_read_summaries_1 (passes->all_regular_ipa_passes);
}

/* Same as execute_pass_list but assume that subpasses of IPA passes
   are local passes.  */

static void
ipa_read_optimization_summaries_1 (opt_pass *pass)
{
  while (pass)
    {
      ipa_opt_pass_d *ipa_pass = (ipa_opt_pass_d *) pass;

      gcc_assert (!current_function_decl);
      gcc_assert (!cfun);
      gcc_assert (pass->type == SIMPLE_IPA_PASS || pass->type == IPA_PASS);

      if (pass->gate (cfun))
	{
	  if (pass->type == IPA_PASS && ipa_pass->read_optimization_summary)
	    {
	      /* If a timevar is present, start it.  */
	      if (pass->tv_id)
		timevar_push (pass->tv_id);

	      pass_init_dump_file (pass);

	      current_pass = pass;
	      ipa_pass->read_optimization_summary ();

	      pass_fini_dump_file (pass);

	      /* Stop timevar.  */
	      if (pass->tv_id)
		timevar_pop (pass->tv_id);
	    }

	  if (pass->sub && pass->sub->type != GIMPLE_PASS)
	    ipa_read_optimization_summaries_1 (pass->sub);
	}
      pass = pass->next;
    }
}

/* Read all the summaries for all_regular_ipa_passes.  */

void
ipa_read_optimization_summaries (void)
{
  pass_manager *passes = g->get_passes ();
  ipa_read_optimization_summaries_1 (passes->all_regular_ipa_passes);
}

/* Same as execute_pass_list but assume that subpasses of IPA passes
   are local passes.  */
void
execute_ipa_pass_list (opt_pass *pass)
{
  do
    {
      gcc_assert (!current_function_decl);
      gcc_assert (!cfun);
      gcc_assert (pass->type == SIMPLE_IPA_PASS || pass->type == IPA_PASS);
      if (execute_one_pass (pass) && pass->sub)
	{
	  if (pass->sub->type == GIMPLE_PASS)
	    {
	      invoke_plugin_callbacks (PLUGIN_EARLY_GIMPLE_PASSES_START, NULL);
	      do_per_function_toporder ((void (*)(function *, void *))
					  execute_pass_list,
					pass->sub);
	      invoke_plugin_callbacks (PLUGIN_EARLY_GIMPLE_PASSES_END, NULL);
	    }
	  else if (pass->sub->type == SIMPLE_IPA_PASS
		   || pass->sub->type == IPA_PASS)
	    execute_ipa_pass_list (pass->sub);
	  else
	    gcc_unreachable ();
	}
      gcc_assert (!current_function_decl);
      symtab->process_new_functions ();
      pass = pass->next;
    }
  while (pass);
}

/* Execute stmt fixup hooks of all passes in PASS for NODE and STMTS.  */

static void
execute_ipa_stmt_fixups (opt_pass *pass,
			 struct cgraph_node *node, gimple **stmts)
{
  while (pass)
    {
      /* Execute all of the IPA_PASSes in the list.  */
      if (pass->type == IPA_PASS
	  && pass->gate (cfun))
	{
	  ipa_opt_pass_d *ipa_pass = (ipa_opt_pass_d *) pass;

	  if (ipa_pass->stmt_fixup)
	    {
	      pass_init_dump_file (pass);
	      /* If a timevar is present, start it.  */
	      if (pass->tv_id)
		timevar_push (pass->tv_id);

	      current_pass = pass;
	      ipa_pass->stmt_fixup (node, stmts);

	      /* Stop timevar.  */
	      if (pass->tv_id)
		timevar_pop (pass->tv_id);
	      pass_fini_dump_file (pass);
	    }
	  if (pass->sub)
	    execute_ipa_stmt_fixups (pass->sub, node, stmts);
	}
      pass = pass->next;
    }
}

/* Execute stmt fixup hooks of all IPA passes for NODE and STMTS.  */

void
execute_all_ipa_stmt_fixups (struct cgraph_node *node, gimple **stmts)
{
  pass_manager *passes = g->get_passes ();
  execute_ipa_stmt_fixups (passes->all_regular_ipa_passes, node, stmts);
}


extern void debug_properties (unsigned int);
extern void dump_properties (FILE *, unsigned int);

DEBUG_FUNCTION void
dump_properties (FILE *dump, unsigned int props)
{
  fprintf (dump, "Properties:\n");
  if (props & PROP_gimple_any)
    fprintf (dump, "PROP_gimple_any\n");
  if (props & PROP_gimple_lcf)
    fprintf (dump, "PROP_gimple_lcf\n");
  if (props & PROP_gimple_leh)
    fprintf (dump, "PROP_gimple_leh\n");
  if (props & PROP_cfg)
    fprintf (dump, "PROP_cfg\n");
  if (props & PROP_ssa)
    fprintf (dump, "PROP_ssa\n");
  if (props & PROP_no_crit_edges)
    fprintf (dump, "PROP_no_crit_edges\n");
  if (props & PROP_rtl)
    fprintf (dump, "PROP_rtl\n");
  if (props & PROP_gimple_lomp)
    fprintf (dump, "PROP_gimple_lomp\n");
  if (props & PROP_gimple_lomp_dev)
    fprintf (dump, "PROP_gimple_lomp_dev\n");
  if (props & PROP_gimple_lcx)
    fprintf (dump, "PROP_gimple_lcx\n");
  if (props & PROP_gimple_lvec)
    fprintf (dump, "PROP_gimple_lvec\n");
  if (props & PROP_cfglayout)
    fprintf (dump, "PROP_cfglayout\n");
}

DEBUG_FUNCTION void
debug_properties (unsigned int props)
{
  dump_properties (stderr, props);
}

/* Called by local passes to see if function is called by already processed nodes.
   Because we process nodes in topological order, this means that function is
   in recursive cycle or we introduced new direct calls.  */
bool
function_called_by_processed_nodes_p (void)
{
  struct cgraph_edge *e;
  for (e = cgraph_node::get (current_function_decl)->callers;
       e;
       e = e->next_caller)
    {
      if (e->caller->decl == current_function_decl)
        continue;
      if (!e->caller->has_gimple_body_p ())
        continue;
      if (TREE_ASM_WRITTEN (e->caller->decl))
        continue;
      if (!e->caller->process && !e->caller->global.inlined_to)
      	break;
    }
  if (dump_file && e)
    {
      fprintf (dump_file, "Already processed call to:\n");
      e->caller->dump (dump_file);
    }
  return e != NULL;
}

#include "gt-passes.h"
