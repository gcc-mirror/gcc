/* PR c++/42054 */
/* { dg-do compile } */

template<int int> struct A; /* { dg-error "two or more" } */
template<int int> struct A; /* { dg-error "two or more" } */
