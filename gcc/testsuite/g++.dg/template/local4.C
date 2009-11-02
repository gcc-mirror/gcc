// PR c++/17413

template <typename T> void foo() {}

int main () {
  struct S {};
  foo<S> (); // { dg-error "match" } 
}
