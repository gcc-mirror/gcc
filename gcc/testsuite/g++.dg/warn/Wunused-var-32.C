// PR c++/84221
// { dg-additional-options -Wunused }

template <class T> struct __attribute((unused)) A { };
template <> struct A<char> { };

void f (void)
{
  A<int> a;   // shouldn't warn
  A<char> ac; // { dg-warning "unused" }
}              
