/* PR target/25254 */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mcmodel=medium -mlarge-data-threshold=1" } */

const struct { int i; int j; } c = { 2, 6 };

const char *
foo (void)
{
  return "OK";
}
