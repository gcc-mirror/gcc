/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-ssa-vops" } */
struct S
{
  int w, x, y, z;
};
struct T
{
  int r;
  struct S s;
};
void bar (struct S, int);
void
foo (int a, struct T b)
{
  struct S x;
  struct S *c = &x;
  if (a)
    c = &b.s;
  bar (*c, a);
}

/* Make sure that .GLOBAL_VAR is not created when there are no
   clobbering calls.  */
/* { dg-final { scan-tree-dump-times "GLOBAL_VAR" 0 "ssa"} } */
/* { dg-final { cleanup-tree-dump "ssa" } } */
