// PR c++/86060
// { dg-options -Wpedantic }

template <typename T> void foo(T (&a)[8]) {
  for (int i : a)		// { dg-warning "range-based" "" { target c++98_only } }
    i;
}
void fn1() { foo<int>; }
