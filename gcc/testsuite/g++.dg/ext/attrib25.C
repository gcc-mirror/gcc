// PR c++/28559

template<typename T> struct A
{
  struct B;
};

struct C
{
  template<typename T> friend struct __attribute__((packed)) A<T>::B; // { dg-warning "uninstantiated" }
};
