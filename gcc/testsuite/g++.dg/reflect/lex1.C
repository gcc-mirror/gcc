// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test lexical aspects of reflection.

constexpr int sz = 2;

void
g ()
{
  int arr[10];
  arr[::sz] = 42;
  int arr2[:> = { 1, 2 };
}
