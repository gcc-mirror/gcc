/* PR debug/66432 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

extern void baz (const char *, const char *) __attribute__ ((__noreturn__));

void
foo (int x, int y[x][x])
{
  if (x < 2)
    baz ("", "");
}

void
bar (void)
{
  int z[2][2] = { { 1, 2 }, { 3, 4 } };
  foo (2, z);
}
