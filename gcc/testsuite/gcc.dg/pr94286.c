/* PR target/94286 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

unsigned a, b;

int
foo (void)
{
  return __builtin_sub_overflow (a, 0x80000000U, &b);
}
