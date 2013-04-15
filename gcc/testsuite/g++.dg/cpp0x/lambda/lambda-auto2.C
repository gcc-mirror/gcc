// PR c++/56901
// { dg-require-effective-target c++11 }

template <typename>
void foo_impl()
{
  int data;
  auto L = [&](){ return data; };
  [&](){ L(); }();
  [&L](){ L(); }();
}

void foo()
{
  foo_impl<int>();
}
