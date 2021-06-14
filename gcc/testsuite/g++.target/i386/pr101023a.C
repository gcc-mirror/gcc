// PR target/101023
// { dg-do run { target { ! ia32 } } }
// { dg-options "-O2 -mtune=opteron -mstackrealign --param=hot-bb-frequency-fraction=1" }

struct S {
  __attribute__((noipa)) int m1 ();
  __attribute__((noipa)) void m2 ();
};
struct T {
  __attribute__((noipa)) virtual S m3 ();
};
struct U : T {
  int u;
  __attribute__((noipa)) U (int);
};
int *a;
S *b;
int c;

int
S::m1 ()
{
  return 0;
}

void
S::m2 ()
{
}

S
T::m3 ()
{
  return S ();
}

U::U (int) : u (4)
{
}

__attribute__((noipa)) int
foo ()
{
  if (a)
    return 0;
  U d(c);
  S *e = b;
  e->m2 ();
  return e->m1();
}

int
main ()
{
  register int r12 __asm ("r12") = 1;
  register int rax __asm ("rax") = 2;
  asm volatile ("" : "+r" (r12), "+r" (rax));
  foo ();
  asm volatile ("" : "+r" (r12));
  if (r12 != 1)
    __builtin_abort ();
  return 0;
}
