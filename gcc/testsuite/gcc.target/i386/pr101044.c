/* PR target/101044 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse3 -mtune=generic" } */
/* { dg-final { scan-assembler-times "neg" 1 } } */

int foo (int x)
{
  return (x < 0) ? x : -x;
}
