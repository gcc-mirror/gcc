/* Tree based linear points-to analysis
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

#ifndef TREE_ALIAS_TYPE_H
#define TREE_ALIAS_TYPE_H

#include "varray.h"

union alias_var_def;
struct aterm_;
struct aterm_list_a;

enum alias_var_kind
{
  ATERM_AVAR
};

struct alias_var_common  GTY (())
{
  enum alias_var_kind kind;
  unsigned int varnum;
  tree decl;
};

struct alias_var_aterm GTY (())
{
  struct alias_var_common common;
  struct aterm_ * GTY((skip (""))) term;
  struct aterm_list_a *GTY ((skip (""))) ptset;
};

union alias_var_def GTY ((desc ("%0.common.kind")))
{
  struct alias_var_common GTY ((tag ("-1"))) common;
  struct alias_var_aterm GTY ((tag ("ATERM_AVAR"))) aterm;
};

typedef union alias_var_def *alias_var;

#define ALIAS_VAR_KIND(x) ((x)->common.kind)
#define ALIAS_VAR_VARNUM(x) ((x)->common.varnum)
#define ALIAS_VAR_DECL(x) ((x)->common.decl)
#define ALIAS_VAR_ATERM(x) ((x)->aterm.term)
#define ALIAS_VAR_PTSET(x) ((x)->aterm.ptset)
union alias_type_def;
typedef union alias_type_def *alias_type;

alias_var alias_var_new_with_aterm (tree, struct aterm_ *);

#endif /* TREE_ALIAS_TYPE_H */
