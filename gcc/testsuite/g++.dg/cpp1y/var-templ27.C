// { dg-do compile { target c++14 } }

namespace A
{
  template <class T> int I = 0;
  template <class T> int I<T*> = 42;
}

int i = A::I<void*>;
