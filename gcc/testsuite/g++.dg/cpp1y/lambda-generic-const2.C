// PR c++/67411
// { dg-do compile { target c++14 } }

template <class T>
void f()
{
  int i = 42;
  [=] {
    const int x = i;
    [&](auto) {
      [=] { return x; }();
    }(1);
  }();
}

int main()
{
  f<int>();
}
