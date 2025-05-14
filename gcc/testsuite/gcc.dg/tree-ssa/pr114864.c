/* { dg-do run } */
/* { dg-options "-O1 -fno-tree-dce -fno-tree-fre" } */

struct a {
  int b;
} const c;
void d(const struct a f) {}
void e(const struct a f) {
  f.b == 0 ? 1 : f.b;
  d(f);
}
int main() {
  e(c);
  return 0;
}
