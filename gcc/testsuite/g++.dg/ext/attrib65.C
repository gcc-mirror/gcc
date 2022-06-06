// PR c++/102399
// { dg-do compile { target c++11 } }
// Test mixing the GNU and standard forms of attributes.

#define EXPORT __attribute__((visibility("default")))

struct [[nodiscard]] EXPORT Foo { Foo(); };
