// PR c++/7229
// { dg-do compile }

namespace A { namespace B { typedef int type; } }
typename A::B<0>::type x; // { dg-error "" }
