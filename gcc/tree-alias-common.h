/* Tree based points-to analysis
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef TREE_ALIAS_COMMON
#define TREE_ALIAS_COMMON

#include "tree-alias-type.h"

/* Alias analysis function pointers.
   Functions implemented by the actual alias analysis algorithms in
   order for them to work with the common points-to structure.  */
struct tree_alias_ops
{
  /* Initialization.
     Called right before we start using the other functions.  */
  void (*init) (struct tree_alias_ops *);

  /* Cleanup. 
     Called when we are finished with the alias analyzer.  */
  void (*cleanup) (struct tree_alias_ops *);

  /* Add variable.
     Called when we want to inform the alias analyzer about a new
     variable we've found.  */
  alias_var (*add_var) (struct tree_alias_ops *, tree);

  /* Add variable equivalent to existing one.
     Called when we want to inform the alias analyzer about a new
     variable that has the same points-to set as an existing
     variable.  */ 
  alias_var (*add_var_same) (struct tree_alias_ops *, tree,
				 alias_var);
  
  /* Process a simple assignment (a = b).
     Called to process simple assignment statements of the form a = b,
     where a and b are both variables.  */
  void (*simple_assign) (struct tree_alias_ops *, alias_var,
			 alias_var);
  /* Process an address assignment (a = &b).
     Called to process address assignment statements of the form a =
     &b, where a and b are both variables.  */
  void (*addr_assign) (struct tree_alias_ops *, alias_var, alias_var);

  /* Process a pointer assignment (a = *b).
     Called to process pointer assignment statements of the form a =
     *b, where a and b are both variables.  */
  void (*ptr_assign) (struct tree_alias_ops *, alias_var, alias_var);

  /* Process an operator assignment (a = op (...))
     Called to process operators of the form a = op(...), where a is a
     variable.  */
  void (*op_assign) (struct tree_alias_ops *, alias_var, varray_type, 
		     tree, bitmap);
  /* Process a heap assignment (a = alloc (...))
     Called to process a heap assignment of the form a = alloc
     (...), where a is a variable, and *alloc is a function that
     returns new memory.  */
  void (*heap_assign) (struct tree_alias_ops *, alias_var);

  /* Process an assignment to a pointer (*a = b)
     Called to process assignment to pointer statements of the form
     *a = b, where a and b are both variables.  */
  void (*assign_ptr) (struct tree_alias_ops *, alias_var, alias_var);

  /* Process a function definition.
     Called to inform the alias analyzer about a new function
     definition.  */
  void (*function_def) (struct tree_alias_ops *, alias_var,
			varray_type, alias_var);

  /* Process a function call.
     Return 1 if we need to assume conservative side-effects.  */
  int (*function_call) (struct tree_alias_ops *, alias_var,
			alias_var, varray_type, bitmap);

  /* Determine if two alias variables may alias.   */
  bool (*may_alias) (struct tree_alias_ops *, alias_var, alias_var);

  /* Determine if two alias variables have the same points-to set.  */
  bool (*same_points_to_set) (struct tree_alias_ops *, alias_var, 
			      alias_var);
  
  /* Determine if the alias variable has an empty points-to set.  */
  bool (*empty_points_to_set) (struct tree_alias_ops *, alias_var);
  
  /* Private data.  */
  void *data;

  /* Interprocedural.  */
  unsigned int ip:1; 

  /* Can do conservative interprocedural analysis if we save the 
   * info.  */
  unsigned int ip_partial:1; 

};

extern struct tree_alias_ops *current_alias_ops;
extern void init_alias_vars (void);
extern bool ptr_may_alias_var (tree, tree);
extern bool same_points_to_set (tree, tree);
extern bool empty_points_to_set (tree);
extern const char *alias_get_name (tree);

#endif /* TREE_ALIAS_COMMON */
