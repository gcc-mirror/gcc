// { dg-do compile }
// Contributed by: <leif dot lonnblad at thep dot lu dot se>
// PR c++/15064: typeid does not form an integral constant expression

#include <typeinfo>

template <typename T>
void dummy() {
  const std::type_info& t = typeid(T);
  const std::type_info& t2 = typeid(float);
}

template void dummy<int>(void);
