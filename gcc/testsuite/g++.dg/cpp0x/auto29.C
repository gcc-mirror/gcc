// PR c++/51401
// { dg-do compile }
// { dg-options "-std=c++11" }

template <int>
struct A
{
  auto i;	// { dg-error "non-static data member declared" }
};

template <int>
struct B
{
  auto i = 0;	// { dg-error "non-static data member declared" }
};

struct C
{
  auto i;	// { dg-error "non-static data member declared" }
};

struct D
{
  auto i = 0;	// { dg-error "non-static data member declared" }
};
