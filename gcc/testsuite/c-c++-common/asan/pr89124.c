/* PR sanitizer/89124 */
/* { dg-do compile } */

static int inline __attribute__ ((always_inline))
foo (int x)
{
  return x + 1;
}

__attribute__ ((no_sanitize_address)) int
bar (int x)
{
  return foo (x);
}
