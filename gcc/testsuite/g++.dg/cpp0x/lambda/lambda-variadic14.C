// PR c++/100109
// { dg-do compile { target c++11 } }

template <int... E>
void f() {
  [] { enum e { e = E }; };	// { dg-error "not expanded" }
}
template void f<>();
