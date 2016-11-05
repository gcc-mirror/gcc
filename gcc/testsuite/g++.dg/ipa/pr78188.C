// { dg-do compile }
// { dg-options "-O2 -fno-exceptions" }

int a;
static void __attribute__((noinline)) foo () { a = 1; }
static void __attribute__((noinline)) foo2 () { a = 2; }

struct X
{
  virtual void bar (int i) { if (!i) { foo (); __builtin_abort (); } }
};

void baz (int i)
{
  if (!i)
     { foo2 (); __builtin_abort (); }
}

X xx;

