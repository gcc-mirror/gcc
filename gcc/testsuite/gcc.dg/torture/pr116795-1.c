/* { dg-do compile } */
/* { dg-options "-O3" } */

volatile int a, b;
int c;
int main() {
  unsigned e = 0;
  for (; e < 2; e++) {
    a && b;
    if (c)
      e = -(c ^ e);
  }
  return 0;
}
