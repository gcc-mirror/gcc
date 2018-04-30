// { dg-do compile { target c++11 } }
// PR c++/84804 ICE instantiating friend with default arg containing a lambda
template<int> struct A
{
  // Note, instantiation injects this into ::, so there can only be one!
  friend void foo(int i = []{ return 0;}()) {}
};

void bar()
{
  A<0> x;
}
