// PR c++/79790
// { dg-options -std=c++17 }

template <int N>
struct array
{
  int a [N];
};

array a = { 1, 2, 3 };  // { dg-error "cannot deduce" }
