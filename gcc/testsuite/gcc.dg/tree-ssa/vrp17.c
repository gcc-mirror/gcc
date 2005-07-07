/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp" } */

extern void abort (void) __attribute__ ((__noreturn__));
union tree_node;
typedef union tree_node *tree;
extern const unsigned char tree_code_length[];
struct tree_common
{
  int code;
};
struct tree_exp
{
  tree operands[1];
};
union tree_node
{
  struct tree_common common;
  struct tree_exp exp;
};
int
gimplify_for_stmt (tree stmt)
{
  if (2 >= tree_code_length[stmt->common.code])
    abort ();
  if (3 >= tree_code_length[stmt->common.code])
    abort ();
}

/* { dg-final { scan-tree-dump-times "== 3" 1 "vrp" } } */
/* { dg-final { cleanup-tree-dump "vrp" } } */

