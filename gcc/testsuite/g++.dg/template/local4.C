// PR c++/17413

template <typename T> void foo() {}

int main () {
  struct S {};
  // We do not simply use "local|match" on line 10 because we want to
  // make sure that "local" appears.
  // { dg-error "local" "local" { target *-*-* } 10 }
  foo<S> (); // { dg-error "trying|match" } 
}
