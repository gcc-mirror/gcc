// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template <auto R = ^^::> class S {};
S s;

template <auto R = ^^int> class S2 {};
S2 s2;
