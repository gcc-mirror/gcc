/* { dg-do run } */

#if (__SIZEOF_LONG_LONG__ == __SIZEOF_POINTER__)
typedef unsigned long long uintptr_t;
#elif (__SIZEOF_LONG__ == __SIZEOF_POINTER__)
typedef unsigned long uintptr_t;
#elif (__SIZEOF_INT__ == __SIZEOF_POINTER__)
typedef unsigned int uintptr_t;
#else
#error Add target support here
#endif

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
