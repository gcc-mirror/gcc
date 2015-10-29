/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fsanitize-undefined-trap-on-error" } */

int __attribute__((noinline,noclone))
h(int a)
{
  return 2 * (a * (__INT_MAX__/2 + 1));
}
int __attribute__((noinline,noclone))
i(int a)
{
  return (2 * a) * (__INT_MAX__/2 + 1);
}
int __attribute__((noinline,noclone))
j(int a, int b)
{
  return (b * a) * (__INT_MAX__/2 + 1);
}
int __attribute__((noinline,noclone))
k(int a, int b)
{
  return (2 * a) * b;
}
int main()
{
  volatile int tem = h(-1);
  tem = i(-1);
  tem = j(-1, 2);
  tem = k(-1, __INT_MAX__/2 + 1);
  return 0;
}
