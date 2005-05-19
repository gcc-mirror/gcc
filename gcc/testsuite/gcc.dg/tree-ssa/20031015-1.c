/* With tree-ssa, gcc.dg/20000724-1.c failed because we missed
   a VOP of x in the asm statement.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-alias1-vops" } */

struct s { int a; };

int
main(void)
{
  struct s x = { 0 };
  asm volatile ("" : : "r" (&x) : "memory");
  return 0;
}

/* The V_MUST_DEF comes from the initial assignment; the V_MAY_DEF
   comes from the asm.  */
/* { dg-final { scan-tree-dump-times "V_MUST_DEF" 1 "alias1" } } */
/* { dg-final { scan-tree-dump-times "V_MAY_DEF" 1 "alias1" } } */
/* { dg-final { cleanup-tree-dump "alias1" } } */
