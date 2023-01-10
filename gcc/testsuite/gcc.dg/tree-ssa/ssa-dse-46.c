/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1" } */

int a;
static long b = 4073709551612, d;
short c;
void foo();
char e(int **f) {
  **f = 0;
  if (a) {
    unsigned long *g = &b;
    unsigned long **h = &g;
    for (; d;) {
      foo();
      for (; c;) {
        unsigned long ***i = &h;
      }
    }
  }
  return 1;
}

/* { dg-final { scan-tree-dump-not "&b" "dse1" } } */
