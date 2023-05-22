// PR c++/109871
// { dg-do compile { target c++11 } }
// { dg-options "" }

#include <initializer_list>

struct vector {
  vector(std::initializer_list<int>); // #1
  vector(int); // #2
};

void f(vector);

int main() {
  f({.blah = 42}); // { dg-error "designated" } previously incorrectly selected #2
}
