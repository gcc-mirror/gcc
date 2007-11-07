// { dg-options -std=c++0x }
// PR c++/33838
template<typename T> struct A
{
  __decltype (T* foo()); // { dg-error "expected|no arguments|accept" }
};
