// { dg-do compile { target c++14 } }

constexpr int
foo (int i)
{
  int a[i] = { }; // { dg-error "7:ISO C\\+\\+ forbids variable length array .a" }
  if (i == 23) return 0;
}

constexpr int j = foo (1);
