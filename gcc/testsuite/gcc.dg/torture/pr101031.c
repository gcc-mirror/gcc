/* { dg-do run } */

int a;
char b, e;
static char *c = &b;
static long d;
void f(void);
void __attribute__((noipa)) h() {
  int g = 0;
  for (; g < 2; ++g) {
    d = *c;
    *c = 1;
    b = 0;
  }
  f();
}
void __attribute__((noipa)) f() {
  if (d++)
    c = &e;
  for (; a;)
    ;
}
int main() {
  h();
  if (b != 0)
    __builtin_abort ();
  return 0;
}
