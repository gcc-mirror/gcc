/* PR rtl-optimization/125209 */
/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-sink" } */

int a, b;
volatile unsigned c;
extern void d(unsigned short);
int e(long f) { return a ? f : 0; }
int main() {
  int g = e(c ^ !b);
  d(g);
  return 0;
}
