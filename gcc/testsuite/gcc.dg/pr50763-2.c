/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef union tree_node *tree;

struct tree_base
{
  int code;
};

struct tree_typed
{
  struct tree_base base;
  tree type;
};

struct tree_common
{
  struct tree_typed typed;
};

struct tree_type_common
{
  tree main_variant;
};

union tree_node
{
  struct tree_base base;
  struct tree_typed typed;
  struct tree_type_common type_common;
};

int std_canonical_va_list_type (tree type)
{
  if (type->base.code)
    type = type->typed.type;
  else
    if (type->typed.type->base.code)
      type = type->typed.type;

  if (type->type_common.main_variant)
    return 1;

  return 0;
}
