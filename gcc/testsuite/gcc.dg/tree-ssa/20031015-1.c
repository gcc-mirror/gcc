/* With tree-ssa, gcc.dg/20000724-1.c failed because we missed
   a VOP of x in the asm statement.  */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-salias-vops" } */

struct s { int a; };

int
main(void)
{
  struct s x = { 0 };
  asm volatile ("" : : "r" (&x) : "memory");
  return 0;
}

/* The VDEF comes from the initial assignment and the asm.  */
/* { dg-final { scan-tree-dump-times "DEF" 2 "salias" } } */
/* { dg-final { cleanup-tree-dump "salias" } } */
