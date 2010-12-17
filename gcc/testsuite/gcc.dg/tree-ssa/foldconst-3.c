/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
typedef const union tree_node *const_tree;
typedef struct
{
}
double_int;
double_int double_int_zext (double_int, unsigned);
enum tree_code
{ ERROR_MARK, IDENTIFIER_NODE, TREE_LIST, BLOCK, ENUMERAL_TYPE, BOOLEAN_TYPE,
    INTEGER_TYPE, ARRAY_TYPE, INTEGER_CST, VAR_DECL, PARM_DECL, RESULT_DECL,
  };
enum tree_code_class
{ tcc_exceptional, tcc_constant, tcc_type, tcc_declaration, tcc_reference, };
struct tree_base
{
  __extension__ enum tree_code code:16;
  unsigned unsigned_flag:1;
};
struct tree_type
{
  unsigned int precision:10;
  union tree_type_symtab
  {
  } symtab;
};
union tree_node
{
  struct tree_base base;
  struct tree_type type;
};
const enum tree_code_class tree_code_type[] =
{ tcc_exceptional, 1, 0, 0, 0, 0, 2, };

int_fits_type_p (const_tree c, const_tree type)
{
  double_int dc, dd;
  {
    if (((enum tree_code) (type)->base.code) == INTEGER_TYPE && ((
								   {
								   __typeof
								   (type) __t
								   = (type);
								   if
								   (tree_code_type
								    [(int)
								     (((enum
									tree_code)
								       (__t)->
								       base.
								       code))]
								    !=
								    (tcc_type))
								   tree_class_check_failed
								   (__t,
								    __FUNCTION__);
								   __t;})->
								 base.
								 unsigned_flag))
      dd = double_int_zext (dd, ((
				   {
				   __typeof (type) __t = (type);
				   if (tree_code_type
				       [(int)
					(((enum tree_code) (__t)->base.
					  code))] !=
				       (tcc_type))
				   tree_class_check_failed (__t,
							    __FUNCTION__);
				   __t;}
				 )->type.precision));
}
}
/* The switch should be switch converted and later constant propagated.  */
/* { dg-final { scan-tree-dump-not "tree_code_type" "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
