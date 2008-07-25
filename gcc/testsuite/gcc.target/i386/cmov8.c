/* PR target/36936 */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -march=i686" } */
/* { dg-final { scan-assembler "cmov\[^8\]" } } */

int
foo (int x)
{
  if (x < 0)
    x = 1;
  return x;
}
