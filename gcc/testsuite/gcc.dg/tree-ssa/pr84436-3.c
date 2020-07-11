/* PR tree-optimization/84436 */
/* { dg-options "-O2 -fdump-tree-switchconv -fdump-tree-optimized" } */

enum a { b, c, d };
int e;
void h(enum a);

void f() {
  enum a g;
  switch (e) {
  case '1':
    g = b;
    break;
  case '2':
    g = c;
    break;
  case '3':
    g = d;
  }
  h(g);
}

/* { dg-final { scan-tree-dump-times ".* \\+ (?:4294967247|65487)" 1 "switchconv" } } */
/* { dg-final { scan-tree-dump-not "switch" "optimized" } } */
