/* Verify warnings fpr accesses to trailing one-element array members
   of a struct that's a member of either a struct or a union.  Both
   are obviously undefined but GCC relies on these hacks so the test
   verifies that -Warray-bounds doesn't trigger for it.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */


typedef union tree_node *tree;

struct tree_exp { int i; tree operands[1]; };

union tree_node
{
  struct tree_exp exp;
};

tree test_nowarn (tree t)
{
  return t->exp.operands[3];    // { dg-bogus "\\\[-Warray-bounds" }
}


typedef struct shrub_node *shrub;

struct shrub_exp { int i; shrub operands[1]; };

struct shrub_node
{
  struct shrub_exp exp;
};

shrub test_warn (shrub s)
{
  return s->exp.operands[3];    // { dg-warning "\\\[-Warray-bounds" "pr96346" { xfail *-*-* } }
}
