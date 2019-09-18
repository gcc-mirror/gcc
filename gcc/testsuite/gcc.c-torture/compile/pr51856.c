/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

struct B { int b1; long long b2, b3; int b4; };
struct C { char c1[40], c2, c3[96]; long long c4[5], c5; char c6[596]; };
void fn1 (long long), fn2 (char *, int), fn4 (void);
int r, fn3 (int, const char *, int, char *, int, int);

void
foo (int t, int u, int v, int w, int x, int y, struct B *z)
{
  char c[512], d[512], e;
  struct C g;
  long long f, h[255];
  struct B j;
  __builtin_bzero (&j, sizeof j);
  if (y > w)
    fn4 ();
  __builtin_bzero (&g, sizeof g);
  g.c5 = h[0];
  fn1 (z ? z->b3 : f);
  g.c2 = y;
  fn2 (d, 256);
  if (fn3 (r, "", e, c, 0, 16))
    fn4 ();
}
