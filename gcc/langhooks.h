/* The lang_hooks data structure.
   Copyright 2001 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_LANG_HOOKS_H
#define GCC_LANG_HOOKS_H

/* The following hooks are documented in langhooks.c.  Must not be
   NULL.  */

struct lang_hooks_for_tree_inlining
{
  union tree_node *(*walk_subtrees) PARAMS ((union tree_node **, int *,
					     union tree_node *(*)
					     (union tree_node **,
					      int *, void *),
					     void *, void *));
  int (*cannot_inline_tree_fn) PARAMS ((union tree_node **));
  int (*disregard_inline_limits) PARAMS ((union tree_node *));
  union tree_node *(*add_pending_fn_decls) PARAMS ((void *,
						    union tree_node *));
  int (*tree_chain_matters_p) PARAMS ((union tree_node *));
  int (*auto_var_in_fn_p) PARAMS ((union tree_node *, union tree_node *));
  union tree_node *(*copy_res_decl_for_inlining) PARAMS ((union tree_node *,
							  union tree_node *,
							  union tree_node *,
							  void *, int *,
							  void *));
  int (*anon_aggr_type_p) PARAMS ((union tree_node *));
};

/* Language-specific hooks.  See langhooks-def.h for defaults.  */

struct lang_hooks
{
  /* Called first, to initialize the front end.  */
  void (*init) PARAMS ((void));

  /* Called last, as a finalizer.  */
  void (*finish) PARAMS ((void));

  /* Called immediately after parsing to clear the binding stack.  */
  void (*clear_binding_stack) PARAMS ((void));

  /* Called to initialize options, before any calls to decode_option.  */
  void (*init_options) PARAMS ((void));

  /* Function called with an option vector as argument, to decode a
     single option (typically starting with -f or -W or +).  It should
     return the number of command-line arguments it uses if it handles
     the option, or 0 and not complain if it does not recognise the
     option.  If this function returns a negative number, then its
     absolute value is the number of command-line arguments used, but,
     in addition, no language-independent option processing should be
     done for this option.  */
  int (*decode_option) PARAMS ((int, char **));

  /* Called when all command line options have been processed.  */
  void (*post_options) PARAMS ((void));

  /* Called to obtain the alias set to be used for an expression or type.
     Returns -1 if the language does nothing special for it.  */
  HOST_WIDE_INT (*get_alias_set) PARAMS ((tree));

  /* Nonzero if TYPE_READONLY and TREE_READONLY should always be honored.  */
  bool honor_readonly;

  struct lang_hooks_for_tree_inlining tree_inlining;

  /* Whenever you add entries here, make sure you adjust langhooks.h
     and langhooks.c accordingly.  */
};

/* Each front end provides its own.  */
extern struct lang_hooks lang_hooks;

#endif /* GCC_LANG_HOOKS_H */
