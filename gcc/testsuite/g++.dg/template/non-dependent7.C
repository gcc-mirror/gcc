// { dg-do compile }

// Origin: Giovanni Bajo <giovannibajo@libero.it>

// PR c++/13092: ICE taking address of member which is non-dependent

struct S
{
  int i;
};

template<int S::*p>
struct X
{};

template <class T>
struct Foo
{
  X<&S::i> x;
};

template struct Foo<void>;
