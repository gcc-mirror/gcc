// PR c++/103497
// { dg-do compile { target c++14 } }

void foo(decltype(auto)... args);  // { dg-error "contains no parameter packs" }

int main() {
  foo();
}
