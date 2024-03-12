/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

static char b = 53;
static unsigned c;
void foo(void);
static int(a)(int d, int e) { return (d ^ e) < 0 ? d : d - e; }
int main() {
  {
    int f = 2;
    c = b;
    b = 0;
    for (; c <= 6;) {
      if (f >= 2)
        f = 0;
      for (; f >= -9; f = a(f, 8))
        if (!(f >= -8 && f <= 0))
          foo();
    }
  }
}


/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */


