/* PR c++/30895 This used to ICE.  */
/* { dg-do compile } */

template<int> struct A {};

template<typename T> struct B
{
  A<T(0i)> a1; /* { dg-error "imaginary constants are a GCC extension" } */
  A<T(0i)> a2; /* { dg-error "imaginary constants are a GCC extension" } */
};
