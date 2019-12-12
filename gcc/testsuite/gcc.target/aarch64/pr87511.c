/* { dg-do assemble } */
/* { dg-options "-Os" } */

int a, d;
struct {
  signed f5 : 26;
  signed f6 : 12;
} b;
signed char c;
void fn1() {
  signed char *e = &c;
  d = a * 10;
  *e = d;
  b.f6 = c;
  b.f5 = 8 <= 3;
}
