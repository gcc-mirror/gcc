// PR c++/102455
// { dg-do compile { target c++14 } }

typedef int v4si;
typedef float v4sf __attribute__ ((vector_size(4)));
constexpr v4sf foo (v4si a) { return (v4sf)a;}
template <class> constexpr v4sf b = foo (v4si {});
