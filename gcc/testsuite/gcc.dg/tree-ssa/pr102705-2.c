/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/102705 */
/* { dg-final { scan-tree-dump-not "foo " "optimized" } } */


void foo(void);
static int b;
static char c;
static short a(short d, short e) { return e == 0 || d && e == 1 ? 0 : d % e; }
int main() {
  b = c = b >= 2 ? 0 : 1 >> b;
  short f = a(0 >= 0 ^ c, 5);
  if (f == c)
    foo();
  a(0, 9);
}
