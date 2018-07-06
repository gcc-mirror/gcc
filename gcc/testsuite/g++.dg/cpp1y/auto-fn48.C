// { dg-do compile { target c++14 } }

template <class T> struct A
{
  static auto fn() { }
  static void f()
  {
    auto x = fn;
  }
};

int main()
{
  A<int>::f();
}
