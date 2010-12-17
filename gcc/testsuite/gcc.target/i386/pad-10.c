/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -march=atom" } */
/* { dg-final { scan-assembler-not "nop" } } */
/* { dg-final { scan-assembler-not "rep" } } */

extern void bar ();

int
foo2 (int z, int x)
{
  if (x == 1)
    {
      bar ();
      return z;
    }
  else
    return x + z;
}
