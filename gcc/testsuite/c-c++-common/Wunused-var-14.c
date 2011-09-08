/* PR c/50179 */
/* { dg-options "-Wunused" } */
/* { dg-do compile } */

void bar (int, ...);

char *
foo (void)
{
  bar (1, (__extension__ ({ static char b[2]; b[0] = 1; b; })));
  bar (1, ({ static char c[2]; c[0] = 1; c; }));
  return ({ static char d[2]; d[0] = 1; d; });
}
