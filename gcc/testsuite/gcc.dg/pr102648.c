/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

void foo();
static char a, c;
static int d, e;
static short b(short f, short g) { return f * g; }
int main() {
  short h = 4;
  for (; d;)
    if (h)
      if(e) {
        if (!b(a & 1 | h, 3))
          c = 0;
        h = 1;
      }
  if (c)
    foo();
}

/* { dg-final { scan-tree-dump-not "foo" "optimized" } } */
