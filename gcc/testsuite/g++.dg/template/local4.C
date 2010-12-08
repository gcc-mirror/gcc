// PR c++/17413

template <typename T> void foo() {} // { dg-message "note" }

int main () {
  struct S {};
  foo<S> (); // { dg-error "match" } 
  // { dg-message "candidate" "candidate note" { target *-*-* } 7 }
}
