/* PR c++/38646 */
/* { dg-do compile } */
/* { dg-options "-std=c++0x" } */

template<int...> struct A;

template<int... N> struct A<N..., N...> /* { dg-error "must be at the end" } */
{
  template<typename> struct B;

  template<typename T> struct B<T*> {};
};
