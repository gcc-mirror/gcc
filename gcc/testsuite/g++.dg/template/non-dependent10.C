// { dg-do compile }

// Origin: Giovanni Bajo <giovannibajo@libero.it>

// Two-phase name lookup for address of member:
// Detecting overloading function error during parsing

struct S
{
  int f(char);
  int f(int);
};

template<int (S::*p)()>
struct X
{};

template <class T>
struct Foo
{
  X<&S::f> x;	// { dg-error "convert|no type" }
};
