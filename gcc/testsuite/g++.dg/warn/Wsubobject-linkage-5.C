// PR c++/86491
// { dg-do compile { target c++11 } }

template <int *> struct NT{};
#line 6 "tM.C"
static int d;
struct D : NT<&d> {};		// { dg-warning "internal linkage" }
