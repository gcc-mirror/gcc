/* [dcl.attr.unused] The attribute may be applied to the declaration of a
   class, a typedef-name, a variable (including a structured binding
   declaration), a non-static data member, a function, an enumeration, or an
   enumerator.  */
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wunused -Wextra" }

class [[maybe_unused]] Test {
  [[maybe_unused]] int a_;
  void b() {};
};

[[maybe_unused]] typedef Test Test2;

[[maybe_unused]] int i;
[[maybe_unused]] void f();
enum [[maybe_unused]] E { e [[maybe_unused]] = 42 };
