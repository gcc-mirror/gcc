/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vrp -fno-tree-fre" } */

int a, b, c;
static int d(short e) { return e || (a && e) ? 0 : a; }
static void f(int e) {
  if (!e) {
    d(0);
    b = d(e);
  }
}
int main() { f(c | 1); }
