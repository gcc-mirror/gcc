// PR c++/116160
// { dg-do compile { target c++20 } }

enum class Blah { b };
void foo() {
  using Blah::b;
  using Blah::b;
  using enum Blah;
}
