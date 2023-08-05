// PR c++/103497
// { dg-do compile { target c++14 } }

void foo(decltype(auto)... args);  // { dg-error "cannot declare a parameter with .decltype.auto.." }

int main() {
  foo();
}
