/* PR c++/103703 - ICE with -Wmismatched-tags with friends and templates
   { dg-do compile }
   { dg-options "-Wall -Wmismatched-tags" } */

template <typename T>
struct A
{
  struct B { };
};

template <typename T>
struct C
{
  friend class A<C>::B;       // { dg-warning "-Wmismatched-tags" "pr102036" { xfail *-*-* } }
};

template struct C<int>;


template <typename T>
struct D
{
  friend class A<D>::B;       // okay, specialized as class below
};

template <>
struct A<long>
{
  class B { };
};

template struct D<long>;
