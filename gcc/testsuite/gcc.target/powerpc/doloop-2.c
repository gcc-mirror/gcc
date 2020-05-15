/* { dg-do compile } */
/* { dg-options "-O2 -fno-unroll-loops" } */

unsigned int
foo1 (unsigned int l, int *a)
{
  unsigned int i;
  for(i = 0;i < l; i++)
    a[i] = i;
  return l;
}

int
foo2 (int l, int *a)
{
  int i;
  for(i = 0;i < l; i++)
    a[i] = i;
  return l;
}

/* The place where we were getting an extra -1 is when converting from 32bits
   to 64bits as the ctr register is used as 64bits on powerpc64.  We should be
   able to do this loop without "add -1/zero_ext/add 1" to the l to get the
   number of iterations of this loop still doing a do-loop.  */

/* { dg-final { scan-assembler-not {(?n)\maddi .*,.*,-1$} } } */
/* { dg-final { scan-assembler-times "bdnz" 2 } } */
/* { dg-final { scan-assembler-times "mtctr" 2 } } */
