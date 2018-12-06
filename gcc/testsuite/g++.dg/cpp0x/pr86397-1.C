// { dg-do compile { target c++11 } }
void e();
template <bool> void f(int() noexcept(e)) {}
template void f<false>(int()); // { dg-error "does not match" "" { target c++17 } }
