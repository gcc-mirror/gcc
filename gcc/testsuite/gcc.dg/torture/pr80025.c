/* PR debug/80025 */
/* { dg-do compile } */
/* { dg-options "-g -ftracer -w" } */

int a;
long int b, c;

long int
foo (void)
{
}

void
bar (int x, short int y, unsigned short int z)
{
}

int
baz (void)
{
  a -= b;
  b = !foo ();
  bar (b ^= (c ^ 1) ? (c ^ 1) : foo (), (__INTPTR_TYPE__) &bar, a);
}
