// { dg-do compile { target c++17_only } }

template <bool B>
  requires B // { dg-error ".requires. does not name a type" }
// { dg-message ".requires. only available with" "" { target *-*-* } .-1 }
void foo() { }
