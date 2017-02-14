/* PR target/36936 */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -march=i386 -mtune=i686" } */
/* { dg-final { scan-assembler "cmov" } } */

extern int foo (int) __attribute__((__target__("arch=i686")));

int
foo (int x)
{
  if (x < 0)
    x = 1;
  return x;
}
