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
#endif
