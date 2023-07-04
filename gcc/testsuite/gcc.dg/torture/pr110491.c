/* { dg-do run } */

int a, c, d, e;
short b;
void f(int *g) { c &= *g; }
void h(void);
void i() {
  a = 1;
  h();
  f(&a);
}
void h() {
  int *j = &c;
  *j = 5;
k:
  for (; 4 + b <= 0;)
    ;
  for (; d;) {
    c = e == 0;
    goto k;
  }
}
int main()
{
  i();
  if (c != 1)
    __builtin_abort ();
  return 0;
}
