// PR c++/91360 - Implement C++20 P1143R2: constinit
// { dg-do compile { target c++11 } }
// Test that 'constinit' isn't recognized pre-C++2a, but '__constinit' is.

constinit int g = 42; // { dg-error ".constinit. does not name a type" "" { target c++17_down } }
__constinit int g2 = 42;
static __constinit int g3 = 42;

void
fn ()
{
  static constinit int x = 69; // { dg-error ".constinit. does not name a type" "" { target c++17_down } }
  static __constinit int x2 = 69;
}
