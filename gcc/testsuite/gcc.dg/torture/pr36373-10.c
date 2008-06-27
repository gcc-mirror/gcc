/* { dg-do run } */

typedef unsigned long uintptr_t;

void __attribute__((noinline))
foo(uintptr_t l)
{
  int *p = (int *)l;
  *p = 1;
}

extern void abort (void);
int main()
{
  int b = 0;
  uintptr_t l = (uintptr_t)&b;
  foo(l);
  if (b != 1)
    abort ();
  return 0;
}
