/* { dg-do run } */
/* { dg-additional-options "-fno-tree-sra -fno-tree-pre -ftree-loop-distribution" } */

struct a {
  unsigned b;
  unsigned c;
} e, *f = &e;
int d = 1;
int main() {
  for (; d; d--) {
    struct a g[] = {{2, 1}, {2, 1}};
    *f = g[1];
  }
  if (e.c != 1)
    __builtin_abort ();
  return 0;
}
