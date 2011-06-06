// PR c++/49264
// { dg-do compile }
// { dg-options "-O2" }

struct B { };
struct A { char a[sizeof (B) + 1]; } a;

static inline void
foo (const B &b)
{
  __builtin_memcpy (&a, &b, sizeof (b));
}

void
bar ()
{
  B c;
  foo (c);
}
