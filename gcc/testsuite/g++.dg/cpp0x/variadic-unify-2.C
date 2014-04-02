// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/40155
// { dg-do compile { target c++11 } }

template <typename T> struct identity
{  typedef T type;  };

template <typename RT, typename... A>
int forward_call(RT (*) (A...), typename identity<A>::type...);

int g (double);

int i = forward_call(&g, 0);
