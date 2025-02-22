/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize -fno-unroll-loops" } */

int *a;
long len;

int
test ()
{
  for (int i = 0; i < len + 1; i++)
    a[i]=i;
}

/* Check we do not split the backedge but keep nice loop form.  */
/* { dg-final { scan-assembler-times "L\[0-9\]+:" 2 } } */
/* Check we do not end up with reg-reg moves from a pre-increment IV
   exit test.  */
/* { dg-final { scan-assembler-not "mov\[lq\]\?\t%\?\[er\].x, %\?\[er\].x" } } */
