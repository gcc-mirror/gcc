/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void foo(void);

static int a, b;
int main() {
  for (; a; ++a) {
    unsigned short d = a;
    if (!(b | d) && d)
      foo();
  }
}

/* { dg-final { scan-tree-dump-not "foo" "evrp" } }  */

