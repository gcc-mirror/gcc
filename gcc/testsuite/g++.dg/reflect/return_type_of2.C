// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::return_type_of.

#include <meta>

using namespace std::meta;

struct S {
  S () {
    int a;
    constexpr auto r = return_type_of (type_of (parent_of (^^a))); // { dg-error "does not have a type" }
    constexpr auto t = return_type_of (parent_of (^^a)); // { dg-error "function or function type with a return type" }
  }
  ~S () {
    int a;
    constexpr auto r = return_type_of (type_of (parent_of (^^a))); // { dg-error "does not have a type" }
    constexpr auto t = return_type_of (parent_of (^^a)); // { dg-error "function or function type with a return type" }
  }
};
