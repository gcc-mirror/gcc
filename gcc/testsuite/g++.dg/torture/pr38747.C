/* { dg-do run } */

extern "C" void abort (void);
inline void *operator new (__SIZE_TYPE__, void *__p) throw () { return __p; }

int __attribute__((noinline))
foo(void)
{
  float f = 0;
  int *i = new (&f) int (1);
  return *(int *)&f;
}

int main()
{
  if (foo() != 1)
    abort ();
  return 0;
}
