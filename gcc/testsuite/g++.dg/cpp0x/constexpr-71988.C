// PR c++/71988
// { dg-do compile { target c++11 } }
// { dg-options "-fdump-ipa-cgraph" }

struct A {};
constexpr A a;
