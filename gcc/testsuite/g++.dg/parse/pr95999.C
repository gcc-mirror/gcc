/* { dg-do compile } */
int a;
enum struct b;
template <typename = enum struct b { c = a d } 
template <> enum struct b { e };  // { dg-error "explicit specialization" }
// { dg-excess-errors "note" }
// { dg-excess-errors "5:" }

