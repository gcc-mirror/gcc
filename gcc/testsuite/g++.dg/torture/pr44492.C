// PR middle-end/44492
// { dg-do run }

struct T { unsigned long p; };
struct S { T a, b, c; unsigned d; };

__attribute__((noinline))
void
bar (const T &x, const T &y)
{
  if (x.p != 0x2348 || y.p != 0x2346)
    __builtin_abort ();
}

__attribute__((noinline))
void
foo (S &s, T e)
{
  unsigned long a = e.p;
  unsigned long b = s.b.p;
  __asm__ volatile ("" : : "rm" (a), "rm" (b));
  bar (e, s.b);
}

int
main ()
{
  S s = { { 0x2345 }, { 0x2346 }, { 0x2347 }, 6 };
  T t = { 0x2348 };
  foo (s, t);
}
