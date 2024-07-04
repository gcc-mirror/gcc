void exit (int);

#ifdef __OPTIMIZE__
extern void undef (void);

void bar (unsigned x) { }
void baz (unsigned x) { }

extern inline void foo (int a, int b)
{
  int c = 0;
  while (c++ < b)
    (__builtin_constant_p (a) ? ((a) > 20000 ? undef () : bar (a)) : baz (a));
}
#else
void foo (int a, int b)
{
}
#endif

int main (void)
{
  foo(10, 100);
  exit (0);
}
