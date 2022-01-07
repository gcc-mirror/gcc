// PR c++/69681
// { dg-do compile { target c++11 } }

void f();
void g();
static_assert(f != g, "");

#if __cpp_constexpr >= 201603L
static_assert([]{} != []{}, "");
#endif
