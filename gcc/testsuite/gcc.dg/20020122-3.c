/* Check that the combination of -Os and -fprefetch-loop-arrays does not
   cause the compiler to crash, which it originally did on i?86.
   Warnings are turned off because not all targets support prefetch.  */

/* { dg-do compile } */
/* { dg-options "-Os -fprefetch-loop-arrays -w" } */
/* { dg-options "-Os -fprefetch-loop-arrays -mtune=pentium3 -w" { target i?86-*-* } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */

int foo (int *p, int n)
{
  int i, r;
  for (i = 0; i < n; i++)
    r += p[i];
  return r;
}
