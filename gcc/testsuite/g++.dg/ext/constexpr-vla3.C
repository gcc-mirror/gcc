// PR c++/69516
// { dg-do compile { target c++14 } }

constexpr int
foo (int n)
{
  __extension__ int a[n] = { 1, 2, 3, 4, 5, 6 }; // { dg-error "array subscript" }
  int z = 0;
  for (int i = 0; i <= n; ++i)
    z += a[i];
  return z;
}

constexpr int n = foo (3); // { dg-message "in .constexpr. expansion of " }
