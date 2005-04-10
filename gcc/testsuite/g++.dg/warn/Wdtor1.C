// PR c++/20145
// { dg-options "-Wnon-virtual-dtor" }
# 1 "t.cc"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "t.cc"
# 1 "include/t.h" 1 3 4
// Declare the template with explicit C++ linkage in case system
// headers have implicit C linkage.
extern "C++" {
template <int> class t
{
  virtual void f();
};
}
# 2 "t.cc" 2

void f(void)
{
  t<1> h;
}

