// { dg-do compile { target c++14 } }
// { dg-additional-options "-Wno-return-type" }

constexpr int
foo (int i)
{
  int a[i] = { }; // { dg-error "7:ISO C\\+\\+ forbids variable length array .a" }
}

constexpr int j = foo (1); // { dg-error "flows off the end|in .constexpr. expansion of" }
