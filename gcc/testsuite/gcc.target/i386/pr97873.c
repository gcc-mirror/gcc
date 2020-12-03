/* PR target/97873 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse3 -mtune=generic" } */
/* { dg-final { scan-assembler-not "test|cmp" } } */

int foo (int x)
{
  return (x < 0) ? -x : x;
}
