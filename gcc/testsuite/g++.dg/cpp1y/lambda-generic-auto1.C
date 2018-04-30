// Related to c++/81525
// { dg-do compile { target c++14 } }

template <class X>
struct A
{
  template <class T>
  static void f()
  {
    [](auto b) {
      auto c = +b;
    }(42);
  }
};

int main()
{
  A<int>::f<int>();
}
