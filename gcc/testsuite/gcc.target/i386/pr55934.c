/* PR inline-asm/55934 */
/* { dg-do compile } */
/* { dg-options "-std=c99 -msse" } */
_Complex float
foo (void)
{
  _Complex float x;
  __asm ("" : "=x" (x)); /* { dg-error "inconsistent .* constraint" } */
  return x;
}
