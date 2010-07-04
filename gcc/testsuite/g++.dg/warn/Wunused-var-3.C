// { dg-options "-Wunused -W" }

#include <typeinfo>
#include <stdarg.h>

void
f1 (int a, ...)
{
  va_list ap;
  va_start (ap, a);
  va_end (ap);
}

int
f2 (int a, ...)
{
  va_list ap;
  va_start (ap, a);
  int i = va_arg (ap, int);
  va_end (ap);
  return i;
}

struct A { int a; A (); virtual ~A (); };
struct B : virtual A { int b; };

struct B *
f3 (struct A *a)
{
  return dynamic_cast <B *> (a);
}

struct A *
f4 (struct B *a)
{
  return static_cast <A *> (a);
}

struct A *
f5 (struct B *a)
{
  return reinterpret_cast <A *> (a);
}

struct A *
f6 (const struct A *a)
{
  return const_cast <A *> (a);
}

int
f7 (long a)
{
  return (int) a;
}

int
f8 (long a)
{
  return int (a);
}

struct C
{
  operator unsigned int() { return 42; }
};

unsigned int
f9 ()
{
  C u;
  return u;
}

struct D
{
  operator int & ();
  operator const int & () const;
};

void foo (int &);
void foo (const int &);

void
f10 ()
{
  const D x = D ();
  foo (x);
}

int
f11 (int a)
{
  return typeid (a) == typeid (int);
}

struct E
{
  int e () {return 0;}
};

template <typename T>
int
f12 (E a)
{
  __decltype (a.e()) i;
  return i;
}

template <> int f12<int> (E);
