// { dg-do compile { target c++11 } }

int operator""_t (long long unsigned); // { dg-message "note: candidate" }

namespace foo {
  int operator""_t (long long unsigned);  // { dg-message "note: candidate" }
}

using namespace foo;
int var = 10_t; // { dg-error "call of overloaded | is ambiguous" }
