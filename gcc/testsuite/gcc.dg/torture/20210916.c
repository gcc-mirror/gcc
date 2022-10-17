/* { dg-do compile } */

typedef union tree_node *tree;
struct tree_base {
  unsigned : 1;
  unsigned lang_flag_2 : 1;
};
struct tree_type {
  tree main_variant;
};
union tree_node {
  struct tree_base base;
  struct tree_type type;
};
tree finish_struct_t, finish_struct_x;
void finish_struct()
{
  for (; finish_struct_t->type.main_variant;)
    finish_struct_x->base.lang_flag_2 = 0;
}
