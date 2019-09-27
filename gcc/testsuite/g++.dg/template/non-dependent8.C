// { dg-do compile }

// Origin: Giovanni Bajo <giovannibajo@libero.it>

// Two-phase name lookup for address of member:
// Detecting error during parsing

struct S
{
  char i;
};

template<int S::*p>
struct X
{};

template <class T>
struct Foo
{
  X<&S::i> x;	// { dg-error "5:could not convert" "" { target c++17 } }
  // { dg-error "could not convert" "" { target c++14_down } .-1 }
};
