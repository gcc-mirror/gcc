// PR c++/40502
// { dg-do compile }
// { dg-options "-O2" }

struct A { char x[12], y[35]; };
struct B { char z[50]; };

inline void
foo (char *dest, const char *__restrict src, __SIZE_TYPE__ n)
{
  __builtin___strncpy_chk (dest, src, n, __builtin_object_size (dest, 0));	// { dg-warning "will always overflow" }
}

void bar (const char *, int);

inline void
baz (int i)
{
  char s[128], t[32];
  bar (s, 0);
  bar (t, i);
  A a;
  B b;
  foo (a.y, b.z, 36);
}

void
test ()
{
  baz (0);
}
