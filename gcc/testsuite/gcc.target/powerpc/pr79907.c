/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O3" } */
/* { dg-require-effective-target powerpc_vsx } */

int foo (short a[], int x)
{
  unsigned int i;
  for (i = 0; i < 1000; i++)
    {
      x = a[i];
      a[i] = (x <= 0 ? 0 : x);
    }
  return x;
}
