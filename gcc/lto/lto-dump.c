/* Functions for LTO dump tool.
   Copyright (C) 2018-2020 Free Software Foundation, Inc.

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
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "function.h"
#include "basic-block.h"
#include "tree.h"
#include "gimple.h"
#include "cfg.h"
#include "tree-cfg.h"
#include "tree-pass.h"
#include "tree-streamer.h"
#include "cgraph.h"
#include "opts.h"
#include "debug.h"
#include "lto-partition.h"
#include "tree-pretty-print.h"
#include "lto-common.h"

/* Stores details of symbols for dumping symbol list.  */

class symbol_entry
{
public:
  symtab_node *node;
  symbol_entry (symtab_node *node_): node (node_)
  {}

  virtual ~symbol_entry ()
  {}

  char* get_name () const
  {
    if (flag_lto_dump_demangle)
      return xstrdup (node->name ());
    else
      return xstrdup (node->asm_name ());
  }

  virtual size_t get_size () const = 0;

  virtual void dump ()
  {
    const char *name = get_name ();
    const char *type_name = node->get_symtab_type_string ();
    const char *visibility = node->get_visibility_string ();
    size_t sz = get_size ();
    printf ("%s  %s  %4" PRIu64 "  %s  ", type_name, visibility, (uint64_t) sz,
	    name);
  }
};

/* Stores variable specific details of symbols for dumping symbol list.  */

class variable_entry: public symbol_entry
{
public:
  variable_entry (varpool_node *node_): symbol_entry (node_)
  {}

  virtual ~variable_entry ()
  {}

  virtual size_t get_size () const
  {
    varpool_node *vnode = dyn_cast<varpool_node *> (node);
    if (DECL_SIZE (vnode->decl) && tree_fits_shwi_p (DECL_SIZE (vnode->decl)))
      return tree_to_shwi (DECL_SIZE (vnode->decl));
    return 0;
  }

  virtual void dump ()
  {
    symbol_entry :: dump ();
    varpool_node *vnode = dyn_cast<varpool_node *> (node);
    vnode->get_constructor ();
    tree value_tree = DECL_INITIAL (vnode->decl);
    if (flag_lto_print_value && value_tree)
      print_generic_expr (stdout, value_tree, TDF_NONE);
    printf ("\n");
  }
};

/* Stores function specific details of symbols for dumping symbol list.  */

class function_entry: public symbol_entry
{
public:
  function_entry (cgraph_node *node_): symbol_entry (node_)
  {}

  virtual ~function_entry ()
  {}

  virtual void dump ()
  {
    symbol_entry :: dump ();
    printf ("\n");
  }

  virtual size_t get_size () const
  {
    cgraph_node *cnode = dyn_cast<cgraph_node *> (node);
    gcc_assert (cnode);

    return (cnode->definition && !cnode->alias)
     ? n_basic_blocks_for_fn (DECL_STRUCT_FUNCTION (cnode->decl))
     : 0;
  }
};

/* Comparing symbols based on size.  */

int size_compare (const void *a, const void *b)
{
  const symbol_entry *e1 = *(const symbol_entry * const*) a;
  const symbol_entry *e2 = *(const symbol_entry * const*) b;

  return e1->get_size () - e2->get_size ();
}

/* Comparing symbols based on name.  */

int name_compare (const void *a, const void *b)
{
  const symbol_entry *e1 = *(const symbol_entry * const*) a;
  const symbol_entry *e2 = *(const symbol_entry * const*) b;

  return strcmp (e1->get_name (), e2->get_name ());
}

/* Dump list of functions and their details.  */

void dump_list_functions (void)
{
  auto_vec<symbol_entry *> v;

  cgraph_node *cnode;
  FOR_EACH_FUNCTION (cnode)
  {
    if (cnode->definition && !cnode->alias)
      cnode->get_untransformed_body ();
    symbol_entry *e = new function_entry (cnode);
    if (!flag_lto_dump_defined || (cnode->definition && !cnode->alias))
      v.safe_push (e);
  }

  if (flag_lto_size_sort)
    v.qsort (size_compare);
  else if (flag_lto_name_sort)
    v.qsort (name_compare);
  if (flag_lto_reverse_sort)
    v.reverse ();

  printf ("Type   Visibility  Size  Name");
  if (flag_lto_print_value)
    printf ("  Value");
  printf ("\n");
  int i=0;
  symbol_entry* e;
  FOR_EACH_VEC_ELT (v, i, e)
    {
      e->dump ();
      delete e;
    }
}

/* Dump list of variables and their details.  */

void dump_list_variables (void)
{
  auto_vec<symbol_entry *> v;

  varpool_node *vnode;
  FOR_EACH_VARIABLE (vnode)
  {
    symbol_entry *e = new variable_entry (vnode);
    if (!flag_lto_dump_defined || vnode->definition)
      v.safe_push (e);
  }

  if (flag_lto_size_sort)
    v.qsort (size_compare);
  else if (flag_lto_name_sort)
    v.qsort (name_compare);
  if (flag_lto_reverse_sort)
    v.reverse ();

  printf ("\n");
  int i=0;
  symbol_entry* e;
  FOR_EACH_VEC_ELT (v, i, e)
    {
      e->dump ();
      delete e;
    }
}

/* Dump symbol table in graphviz format.  */
void dump_symtab_graphviz (void)
{
  symtab->dump_graphviz (stdout);
}

/* Dump symbol list.  */

void dump_list (void)
{
  dump_list_functions ();
  dump_list_variables ();
  return;
}

/* Dump specific variables and functions used in IL.  */
void dump_symbol ()
{
  symtab_node *node;
  printf ("Symbol: %s\n", flag_lto_dump_symbol);
  FOR_EACH_SYMBOL (node)
    {
      if (!strcmp (flag_lto_dump_symbol, node->name ()))
	{
	  node->debug ();
	  printf ("\n");
	}
    }
  return;
}

/* Dump specific gimple body of specified function.  */
void dump_body ()
{
  int flag = 0;
  dump_flags_t flags = TDF_NONE;
  if (flag_dump_level)
    flags = parse_dump_option (flag_dump_level, NULL);
  if (flags == TDF_ERROR)
  {
    error_at (input_location, "Level not found, use none, slim, blocks, vops.");
    return;
  }
  cgraph_node *cnode;
  FOR_EACH_FUNCTION (cnode)
    if (cnode->definition
	&& !cnode->alias
	&& !strcmp (cnode->name (), flag_dump_body))
      {
	printf ("Gimple Body of Function: %s\n", cnode->name ());
	cnode->get_untransformed_body ();
	debug_function (cnode->decl, flags);
	flag = 1;
      }
  if (!flag)
    error_at (input_location, "Function not found.");
  return;
}

/* List of command line options for dumping.  */
void dump_tool_help ()
{
  const char *msg =
    "Usage: lto-dump [OPTION]... SUB_COMMAND [OPTION]...\n\n"
    "LTO dump tool command line options.\n\n"
    "  -list [options]           Dump the symbol list.\n"
    "    -demangle               Dump the demangled output.\n"
    "    -defined-only           Dump only the defined symbols.\n"
    "    -print-value            Dump initial values of the variables.\n"
    "    -name-sort              Sort the symbols alphabetically.\n"
    "    -size-sort              Sort the symbols according to size.\n"
    "    -reverse-sort           Dump the symbols in reverse order.\n"
    "  -symbol=                  Dump the details of specific symbol.\n"
    "  -objects                  Dump the details of LTO objects.\n"
    "  -callgraph                Dump the callgraph in graphviz format.\n"
    "  -type-stats               Dump statistics of tree types.\n"
    "  -tree-stats               Dump statistics of trees.\n"
    "  -gimple-stats             Dump statistics of gimple statements.\n"
    "  -dump-body=               Dump the specific gimple body.\n"
    "  -dump-level=              Deciding the optimization level of body.\n"
    "  -help                     Display the dump tool help.\n";

  fputs (msg, stdout);
  return;
}

unsigned int
lto_option_lang_mask (void)
{
  return CL_LTODump;
}

/* Functions for dumping various details in LTO dump tool are called
   in lto_main(). The purpose of this dump tool is to analyze the LTO
   object files.  */

void
lto_main (void)
{
  quiet_flag = true;
  if (flag_lto_dump_tool_help)
    dump_tool_help ();

  /* LTO is called as a front end, even though it is not a front end.
     Because it is called as a front end, TV_PHASE_PARSING and
     TV_PARSE_GLOBAL are active, and we need to turn them off while
     doing LTO.  Later we turn them back on so they are active up in
     toplev.c.  */

  /* Initialize the LTO front end.  */
  lto_fe_init ();
  g_timer = NULL;
  /* Read all the symbols and call graph from all the files in the
     command line.  */
  read_cgraph_and_symbols (num_in_fnames, in_fnames);

  /* Dump symbol list.  */
  if (flag_lto_dump_list)
    dump_list ();
  else if (flag_lto_dump_symbol)
    {
      /* Dump specific variables and functions used in IL.  */
      dump_symbol ();
    }
  else if (flag_lto_gimple_stats)
    {
      /* Dump gimple statement statistics.  */
      cgraph_node *node;
      FOR_EACH_DEFINED_FUNCTION (node)
	node->get_untransformed_body ();
      if (!GATHER_STATISTICS)
	warning_at (input_location, 0,
		    "Not configured with "
		    "%<--enable-gather-detailed-mem-stats%>.");
      else
	dump_gimple_statistics ();
    }
  else if (flag_lto_tree_stats)
    {
      /* Dump tree statistics.  */
      if (!GATHER_STATISTICS)
	warning_at (input_location, 0,
		    "Not configured with "
		    "%<--enable-gather-detailed-mem-stats%>.");
      else
	{
	  printf ("Tree Statistics\n");
	  dump_tree_statistics ();
	}
    }
  else if (flag_dump_body)
    {
      /* Dump specific gimple body of specified function.  */
      dump_body ();
      return;
    }
  else if (flag_dump_callgraph)
    {
      dump_symtab_graphviz ();
      return;
    }
}
