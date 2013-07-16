// PR c++/54276
// { dg-do link { target c++11 } }

template <typename T>
void foo(T)
{
  static int x = 1;
  auto f = [] { return x + 1; };
  f();
}

int main()
{
  foo(4);
}
