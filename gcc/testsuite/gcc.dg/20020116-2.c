/* This testcase ICEd on sparc64 because -mcpu=supersparc and implicit
   -m64 resulted in MASK_V8 and MASK_V9 to be set at the same time.  */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-options "-mcpu=supersparc" { target sparc*-*-* } } */

void bar (long *x, long *y);

void foo (int x, long *y, long *z)
{
  int i;

  for (i = x - 1; i >= 0; i--)
    {
      bar (z + i * 3 + 1, y);
      bar (z + i * 3 + 2, y);
    }
}
