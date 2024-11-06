/* PR rtl-optimization/115933 */
/* { dg-do run } */
/* { dg-options "-O1 -fno-tree-loop-optimize -ftree-vrp -fno-tree-ch -fgcse" } */

int a, b;
unsigned c() {
  int d, e = d = 2;
  if (a < 0)
    for (e = 0; e < 1; e++)
      d = 0;
  b = e;
  return d;
}
int main() {
  c();
  if (b != 2)
    __builtin_abort();
  return 0;
}
