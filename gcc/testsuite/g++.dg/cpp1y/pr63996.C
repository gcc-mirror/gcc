// { dg-do compile { target c++14 } }

constexpr int
foo (int i)
{
  int a[i] = { };
}

constexpr int j = foo (1); // { dg-error "is not a constant expression" }

