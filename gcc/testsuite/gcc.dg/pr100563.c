/* { dg-do compile } */
/* { dg-options "-Og" } */
unsigned long long e(void);
void f(int);
void a() {
  short b = -1, c = (int)&b;
  unsigned long long d = e();
  f(b >= d);
}
