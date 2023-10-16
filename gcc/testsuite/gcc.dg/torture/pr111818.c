/* { dg-do compile } */

static void foo(const volatile unsigned int x, void *p)
{
  __builtin_memcpy(p, (void *)&x, sizeof x);
}

void bar(void *number)
{
  foo(0, number);
}
