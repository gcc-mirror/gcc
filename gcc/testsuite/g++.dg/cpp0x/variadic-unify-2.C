// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/40155
// { dg-options "-std=c++11" }
// { dg-do compile }

template <typename T> struct identity
{  typedef T type;  };

template <typename RT, typename... A>
int forward_call(RT (*) (A...), typename identity<A>::type...);

int g (double);

int i = forward_call(&g, 0);
