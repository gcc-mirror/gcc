// Test for Core 2189.
// { dg-do compile { target c++11 } }

template <class T>
using Fn = void (*)(T);

struct A
{
  template <class T>
  operator Fn<T>();
};

int main()
{
  A()(42);
}
