// PR c++/84333
// { dg-options -Wno-pedantic }

template<typename T> int foo()
{
  return sizeof(T) > 1 ? : 1;
}
