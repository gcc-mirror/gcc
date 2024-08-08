// C++26 P2747R2 - constexpr placement new
// { dg-do compile { target c++26 } }

#include "../cpp2a/construct_at.h"

struct S {
  constexpr S () : a (42), b (43) {}
  constexpr S (int c, int d) : a (c), b (d) {}
  int a, b;
};
struct T {
  int a, b;
};

constexpr bool
foo ()
{
  std::allocator<int> a;
  auto b = a.allocate (3);
  new (b + 1) int[] {2, 3};	// { dg-error "" "" { xfail *-*-* } }
  a.deallocate (b, 3);
  return true;
}

constexpr bool
bar ()
{
  std::allocator<int> a;
  auto b = a.allocate (3);
  new (b) int[] {1, 2, 3, 4};	// { dg-error "array subscript value '3' is outside the bounds of array 'heap ' of type 'int \\\[3\\\]'" }
  a.deallocate (b, 3);
  return true;
}

constexpr bool
baz ()
{
  std::allocator<int> a;
  auto b = a.allocate (2);
  new (b) long (42);		// { dg-error "accessing value of 'heap ' through a 'long int' glvalue in a constant expression" }
  a.deallocate (b, 2);
  return true;
}

constexpr bool a = foo ();
constexpr bool b = bar ();
constexpr bool c = baz ();
