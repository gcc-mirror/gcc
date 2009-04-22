typedef union tree_node *tree;
enum tree_code { EXCESS_PRECISION_EXPR };
enum built_in_function { BUILT_IN_ACOS, BUILT_IN_FPCLASSIFY, BUILT_IN_ISFINITE };
struct tree_base {
    __extension__ enum tree_code code : 16;
    unsigned side_effects_flag : 1;
};
struct tree_exp {
    tree     operands[1];
};
struct tree_function_decl {
    __extension__ enum built_in_function function_code : 11;
    unsigned static_ctor_flag : 1;
};
union tree_node {
    struct tree_base base;
    struct tree_function_decl function_decl;
    struct tree_exp exp;
};
static tree
convert_arguments (tree fundecl)
{
  tree val = (void *)0;
  unsigned int parmnum;
  unsigned char type_generic_remove_excess_precision = 0;
  switch (((fundecl)->function_decl.function_code))
    {
      case BUILT_IN_ISFINITE:
      case BUILT_IN_FPCLASSIFY:
	  type_generic_remove_excess_precision = 1;
    }
  for (parmnum = 0;; ++parmnum)
    if (((enum tree_code) (val)->base.code) == EXCESS_PRECISION_EXPR
	&& !type_generic_remove_excess_precision)
      val = ((val)->exp.operands[0]);
  return val;
}
tree
build_function_call_vec (tree function)
{
  return convert_arguments (function);
}

