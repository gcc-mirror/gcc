// PR c++/28559
// { dg-do compile { target c++11 } }

template<typename T> struct A
{
  struct B;
};

struct C
{
  template<typename T> friend struct [[gnu::packed]] A<T>::B; // { dg-warning "uninstantiated" }
};
