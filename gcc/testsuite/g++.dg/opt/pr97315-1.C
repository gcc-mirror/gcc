/* { dg-do compile } */
/* { dg-options "-O3 -fno-exceptions -fno-short-enums" } */

typedef struct tree_node *tree;
enum tree_code { RECORD_TYPE, QUAL_UNION_TYPE };
enum tree_code_class {};
struct tree_base {
  tree_code code : 16;
};
struct tree_node {
  tree_base base;
};
extern tree_code_class tree_code_type[];
void tree_check_failed() __attribute__((__noreturn__));
tree tree_check3(tree __t, tree_code __c1, tree_code __c3) {
  if (__t->base.code != __c1 && __t->base.code != __c3)
    tree_check_failed();
  return __t;
}
tree add_type_duplicate_type;
void add_type_duplicate() {
  if (tree_code_type[add_type_duplicate_type->base.code])
    if (add_type_duplicate_type->base.code == RECORD_TYPE)
      for (;
           tree_check3(add_type_duplicate_type, RECORD_TYPE, QUAL_UNION_TYPE);)
        tree_check3(add_type_duplicate_type, RECORD_TYPE, QUAL_UNION_TYPE);
}
