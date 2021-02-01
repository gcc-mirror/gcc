// { dg-do compile { target c++20 } }

template <class U> struct A
{
  template <class T> void spam(decltype([]{}) *s = nullptr) { }
};

void foo()
{
  A<int>().spam<int>();
}
