// { dg-do compile { target c++14 } }

// PR 83739, deduced range-for in lambda in template

template <bool> void f()
{
  int x[2];
  auto delegate = [](auto & foo)
  {
    for (auto bar : foo);
  };
  delegate(x);
}
int main() {
  f<true>();
}
