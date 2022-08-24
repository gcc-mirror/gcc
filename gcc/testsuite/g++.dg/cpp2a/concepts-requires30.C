// PR c++/105304
// { dg-do compile { target c++20 } }
// { dg-additional-options "-Wall -Wsequence-point" }

struct A { };

int main() {
  if (requires { A(); })
    ;
}
