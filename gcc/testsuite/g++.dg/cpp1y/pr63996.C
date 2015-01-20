// { dg-do compile { target c++14 } }

constexpr int
foo (int i)
{
  int a[i] = { }; // { dg-error "forbids variable length" }
}

constexpr int j = foo (1); // { dg-error "flows off the end" }

