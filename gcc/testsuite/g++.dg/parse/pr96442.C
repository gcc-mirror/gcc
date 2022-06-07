/* { dg-do compile } */
/* { dg-options "-O2" } */
enum struct a : struct {};
template <class b> enum class a : class c{};
enum struct a {b};
// { dg-excess-errors "" }
