/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void foo(void);

static int b, c, d, e, f, ah;
static short g, ai, am, aq, as;
static char an, at, av, ax, ay;
static char a(char h, char i) { return i == 0 || h && i == 1 ? 0 : h % i; }
static void ae(int h) {
  if (a(b, h))
    foo();

}
int main() {
  ae(1);
  ay = a(0, ay);
  ax = a(g, aq);
  at = a(0, as);
  av = a(c, 1);
  an = a(am, f);
  int al = e || ((a(1, ah) && b) & d) == 2;
  ai = al;
}

/* We should eliminate the call to foo.  */
/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
