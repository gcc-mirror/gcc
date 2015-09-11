/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */


extern void abort (void) __attribute__ ((__noreturn__));
union tree_node;
typedef union tree_node *tree;
enum tree_code
{
  ERROR_MARK,
  COND_EXPR = 42,
};
extern const unsigned char tree_code_length[];
struct tree_common
{
  enum tree_code code:8;
};
union tree_node
{
  struct tree_common common;
};
void
blah (tree t)
{
  if (t->common.code != COND_EXPR)
    abort ();
  if (1 >= tree_code_length[t->common.code])
    abort ();

}

/* { dg-final { scan-tree-dump-times "tree_code_length.42." 1 "vrp1" } } */

