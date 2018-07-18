// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S { int a[0]; };
struct T : public S { int b[0]; int c; };
static_assert(!__is_standard_layout (T), "");
