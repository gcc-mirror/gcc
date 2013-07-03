// PR c++/57771
// { dg-do compile }

template <int N>
struct S {};

S <static_cast <int> (4>>2)> s1;
S <reinterpret_cast <int> (4>>2)> s2;
