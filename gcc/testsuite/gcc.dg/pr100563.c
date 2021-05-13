/* { dg-do compile } */
/* { dg-options "-Og -Wno-pointer-to-int-cast" } */
unsigned long long e(void);
void f(int);
void a() {
  short b = -1, c = (int)&b;
  unsigned long long d = e();
  f(b >= d);
}
