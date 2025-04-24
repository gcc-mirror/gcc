/* PR target/119873 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

extern int foo (int, int, int, long long, int);

int
bar (int u, int v, int w, long long x, int y)
{
  [[gnu::musttail]] return foo (u, v, w, x + 1, y - 1);	/* { dg-error "cannot tail-call: target is not able to optimize the call into a sibling call" } */
}

extern int baz (int, int, int, int, int);

int
qux (int u, int v, int w, int x, int y)
{
  [[gnu::musttail]] return baz (u, v, w, x, y + 1);	/* { dg-error "cannot tail-call: target is not able to optimize the call into a sibling call" } */
}

extern int corge (int, int, int, int, unsigned short);

int
garply (int u, int v, int w, int x, unsigned short y)
{
  [[gnu::musttail]] return corge (u, v, w, x, y + 1);	/* { dg-error "cannot tail-call: target is not able to optimize the call into a sibling call" } */
}
