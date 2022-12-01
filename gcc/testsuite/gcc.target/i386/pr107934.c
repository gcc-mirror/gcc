/* { dg-do compile } */
/* { dg-options "-O2 -mtune=knl -ffinite-math-only -msse2" } */

int
foo (__bf16 bf)
{
  return bf;
}
