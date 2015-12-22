// PR c++/67257
// { dg-do compile { target c++14 } }

template <typename> struct plus;
template <typename> struct A {
  template <typename T> auto operator()(T);
} foldl;			// { dg-error "" }
void foo() { foldl<plus<int>>(0); }

// { dg-prune-output "not declared" }
// { dg-prune-output "expected" }
