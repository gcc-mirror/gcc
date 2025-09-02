// PR c++/121539
// { dg-do run }

#include <cstdarg>

#if __cplusplus >= 201103L
#define I {}
#else
#define I 0
#endif

void foo (int = I...);				// { dg-warning "omission of ',' before varargs '...' is deprecated" "" { target c++26 } }
struct S {
  void foo (int = I...);			// { dg-warning "omission of ',' before varargs '...' is deprecated" "" { target c++26 } }
  void bar (int...);				// { dg-warning "omission of ',' before varargs '...' is deprecated" "" { target c++26 } }
};

void
foo (int a, ...)
{
  if (a == 42)
    {
      va_list ap;
      va_start (ap, a);
      if (va_arg (ap, double) != 15.0)
	__builtin_abort ();
      va_end (ap);
    }
  else if (a != 0)
    __builtin_abort ();
}

void
S::foo (int a, ...)
{
  if (a == 43)
    {
      va_list ap;
      va_start (ap, a);
      if (va_arg (ap, double) != 16.0)
	__builtin_abort ();
      va_end (ap);
    }
  else if (a != 0)
    __builtin_abort ();
}

void
S::bar (int a = I...)				// { dg-warning "omission of ',' before varargs '...' is deprecated" "" { target c++26 } }
{
  if (a == 44)
    {
      va_list ap;
      va_start (ap, a);
      if (va_arg (ap, double) != 17.0)
	__builtin_abort ();
      va_end (ap);
    }
  else if (a != 0)
    __builtin_abort ();
}

int
main ()
{
  S s;
  foo ();
  foo (0);
  foo (42, 15.0);
  foo (42, 15.0, 128LL);
  s.foo ();
  s.foo (0);
  s.foo (43, 16.0);
  s.foo (43, 16.0, 129ULL);
  s.bar ();
  s.bar (0);
  s.bar (44, 17.0);
  s.bar (44, 17.0, 130L);
}
