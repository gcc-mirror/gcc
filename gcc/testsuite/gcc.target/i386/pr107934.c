/* { dg-do compile } */
/* { dg-options "-O2 -mtune=icelake-server -ffinite-math-only -msse2" } */

int
foo (__bf16 bf)
{
  return bf;
}
