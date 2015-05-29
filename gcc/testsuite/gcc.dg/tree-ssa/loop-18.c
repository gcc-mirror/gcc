/* A test for # of iterations estimation.  We know that I does not overflow,
   thus we can perform strength reduction (even though the 32-bit variable
   i is first extended to 64-bit type).  */

/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */

unsigned bar(void);

void foo(unsigned *p, unsigned n)
{
  unsigned i;

  for (i = 0; i < n; i++)
    p[i] = bar ();
}

/* Check that the memory reference was replaced with MEM, and that there is no
   multiplication.  */

/* { dg-final { scan-tree-dump-times "MEM" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\[^\\n\\r\]*= \\* " 0 "optimized" } } */

