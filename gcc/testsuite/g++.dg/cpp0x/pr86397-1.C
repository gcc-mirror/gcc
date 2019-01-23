// { dg-do compile { target c++11 } }
// { dg-options "-fdelete-null-pointer-checks" }
void e();
template <bool> void f(int() noexcept(e)) {}
template void f<false>(int()); // { dg-error "does not match" "" { target c++17 } }
