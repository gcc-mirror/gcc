// PR c++/84221
// { dg-additional-options -Wunused }

template <class T> struct __attribute((unused)) A { };

void f (void)
{
  A<int> a;   // shouldn't warn
}              
