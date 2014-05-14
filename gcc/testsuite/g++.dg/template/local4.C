// PR c++/17413
// { dg-options -std=c++98 }

template <typename T> void foo() {} // { dg-message "note" }

int main () {
  struct S {};
  foo<S> (); // { dg-error "(match|template argument for|trying to instantiate)" } 
}
