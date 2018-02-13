// PR c++/84333
// { dg-options -Wno-pedantic }

template<typename> int foo()
{
  return sizeof(int) > 1 ? : 1;
}
