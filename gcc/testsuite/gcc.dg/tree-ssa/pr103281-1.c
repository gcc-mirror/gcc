/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optmization/103281 */

void foo(void);

static unsigned b;

int main() {
  for (; b < 3; b++) {
    char c = b;
    char a = c ? c : c << 1;
    if (!(a < 1 ^ b))
      foo();
  }
}

/* the call to foo should be optimized away. */
/* { dg-final { scan-tree-dump-not "foo " "optimized" } } */
