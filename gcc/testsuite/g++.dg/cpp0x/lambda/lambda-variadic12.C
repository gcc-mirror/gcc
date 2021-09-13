// PR c++/99583
// { dg-do compile { target c++11 } }

void f(...);

template <bool... B>
void g() {
 f([]() noexcept(B) {} ...);
}
