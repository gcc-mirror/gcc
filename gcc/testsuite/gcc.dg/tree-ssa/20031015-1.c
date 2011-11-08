/* With tree-ssa, gcc.dg/20000724-1.c failed because we missed
   a VOP of x in the asm statement.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-alias-vops" } */

struct s { int a; };

int
main(void)
{
  struct s x = { 0 };
  asm volatile ("" : : "r" (&x) : "memory");
  return 0;
}

/* The VDEF comes from the initial assignment, the asm, and the clobber.  */
/* { dg-final { scan-tree-dump-times "DEF" 3 "alias" } } */
/* { dg-final { cleanup-tree-dump "alias" } } */
