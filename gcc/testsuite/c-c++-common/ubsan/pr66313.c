/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fsanitize-undefined-trap-on-error" } */

int __attribute__((noinline,noclone))
f(int a, int b, int c)
{
  return a * b + a * c;
}
int __attribute__((noinline,noclone))
g(int a)
{
  return a * (__INT_MAX__/2) + a * (__INT_MAX__/2 + 2);
}
int __attribute__((noinline,noclone))
h(int a, int b)
{
  return a * (__INT_MAX__/2 + 1) + b * (__INT_MAX__/2 + 1);
}
int main()
{
  volatile int tem = f(0, __INT_MAX__, __INT_MAX__);
  tem = f(-1, __INT_MAX__/2 + 1, __INT_MAX__/2 + 1);
  tem = g(-1);
  tem = h(-1, -1);
  return 0;
}
