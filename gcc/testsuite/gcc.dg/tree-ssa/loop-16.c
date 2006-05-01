/* A test for # of iterations estimation.  We know that the loop is executed
   at most 100 times, thus the (32-bit) induction variables do not overflow,
   and we may use 64-bit variable to represent them.  */

/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-do compile { target x86_64-*-* } } */

unsigned a[100];

void foo(unsigned n)
{
  unsigned i;

  for (i = 0; i < n; i++)
    a[i] = 4 * i;
}

/* Check that the memory reference was replaced with MEM, and that there is no
   multiplication.  */

/* { dg-final { scan-tree-dump-times "MEM" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\[^\\n\\r\]*= \\* " 0 "optimized" } } */

/* { dg-final { cleanup-tree-dump "optimized" } } */
