/* PR target/77587 */
/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-require-weak-override "" } */

void
foo (long x, long y, long z)
{
}

void bar (long x, long y, long z) __attribute__ ((weak, alias ("foo")));

void
baz (long x, long y, long z)
{
  bar (0, 0, 0);
}

int
main ()
{
  baz (0, 0, 0);
  return 0;
}
