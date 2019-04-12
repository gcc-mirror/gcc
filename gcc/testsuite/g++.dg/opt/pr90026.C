// PR rtl-optimization/90026
// { dg-do compile }
// { dg-options "-fnon-call-exceptions -ftracer -O2 -w" }

typedef __SIZE_TYPE__ size_t;
struct S { int *b; ~S () { delete b; } };
void bar ();
char c[sizeof (int)];

void *
operator new (size_t, void *)
{
  __builtin_unreachable ();
}

void
foo ()
{
  S a;
  if (a.b)
    a.b = new int ();
  bar ();
  new (c) int ();
}
