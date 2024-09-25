/* { dg-do run } */
/* { dg-additional-options "-fno-forward-propagate -fno-tree-ch" } */
int a, d, e;
signed char b = -1, c, f;
int main() {
  int g;
  for (; d < 1; d++) {
    g = b;
    for (; c; c = g)
      ;
  }
  f = g;
  for (; e < 1; e++)
    if (g >= a)
      __builtin_abort();
  return 0;
}
