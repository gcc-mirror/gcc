// PR c++/103600
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

#include <typeinfo>

struct S { int i; };
namespace {
  struct T { int i; };
};
constexpr bool a = &typeid (int) == &typeid (int);
constexpr bool b = &typeid (int) == &typeid (long);
constexpr bool c = &typeid (double) != &typeid (int);
constexpr bool d = &typeid (S) != &typeid (T);
static_assert (a && !b && c && d, "");
