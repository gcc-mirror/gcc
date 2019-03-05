/* { dg-do run } */
/* { dg-options "-ftree-tail-merge -Wno-div-by-zero -O2 -fno-dce -fno-isolate-erroneous-paths-dereference -fno-tree-dce -fno-tree-vrp" } */
/* { dg-require-effective-target ptr_eq_long } */

int b, c, d, e;

__attribute__ ((noinline, noclone))
int foo (short f)
{
  f %= 0;
  return f;
}

int
main (void)
{
  b = (unsigned char) __builtin_parity (d);
  e ? foo (0) : (long) &c;
  return 0;
}
