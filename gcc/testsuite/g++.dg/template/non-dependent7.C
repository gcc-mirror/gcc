// { dg-do compile }

// Origin: Giovanni Bajo <giovannibajo@libero.it>

// Two-phase name lookup for address of member:
// Overloading function

struct S
{
  int f();
  int f(int);
};

template<int (S::*p)()>
struct X
{};

template <class T>
struct Foo
{
  X<&S::f> x;
};
