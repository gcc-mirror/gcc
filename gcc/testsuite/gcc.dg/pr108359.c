/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR test case.  */
int b = 10;
int c;
char e;
void foo();
static char(a)(char f, char g) { return f && g == 1 ? 0 : f % g; }
short(d)(short f, short g) { return f * g; }
int main() {
  short h;
  int i;
  unsigned j;
  h = d(b && c, 5);
  j = h;
  i = a(h, 237);
  unsigned k = i;
  e = i < 0 || k >= 32 ? 0 : i >> k;
  if (e) {
    c = 0;
    foo();
  }
}


/* Also Check that small ranges are broken down and optimized properly
   This function should never call foo ().  */

int otherfunc (int x, int z) {
  if (x < 1 || x > 6 )
    return 0;

  if (x == z)
    {
    if (x >> z > 0)
      foo ();
    if (x * z > 26 && x * z < 35)
      foo ();
    if (x + z == 5)
      foo ();
    if ((x + z) % 2 == 1)
      foo ();
    if (x / z != 1)
      foo ();

    }
  return 0;
}


/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
