/* PR debug/89528 */
/* { dg-do run } */
/* { dg-options "-g" } */

#include <stdio.h>

char b;
int d, e;
static int i = 1;
void a(int l) { printf("", l); }
char c(char l) { return l || b && l == 1 ? b : b % l; }
short f(int l, int m) { return l * m; }
short g(short l, short m) { return m || l == 767 && m == 1; }
int h(int l, int m) { return (l ^ m & l ^ (m & 647) - m ^ m) < m; }
static int j(int l) { return d == 0 || l == 647 && d == 1 ? l : l % d; }
short k(int l) { return l >= 2 >> l; }
void optimize_me_not() { asm(""); }
static short n(void) {
  int l_1127 = ~j(9 || 0) ^ 65535;
  optimize_me_not(); /* { dg-final { gdb-test . "l_1127+1" "-65534" } } */
  f(l_1127, i && e ^ 4) && g(0, 0);
  e = 0;
  return 5;
}
int main() { n(); }
