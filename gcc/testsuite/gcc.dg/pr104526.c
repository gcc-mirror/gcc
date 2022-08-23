/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void foo(void);

static int a, b = 1, *c = &b;
int main() {
  for (; a; a--) {
    int d = 2 >> (1 / *c);
    if (!d)
      foo();
  }
}

/* { dg-final { scan-tree-dump-not "foo" "evrp" } } */
