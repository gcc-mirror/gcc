/* PR c++/31517. This used to ICE.  */
/* { dg-do compile } */

template<typename> struct A
{
  static const int i=0;
};

template<typename T> const int A<T>::i = T()=0; /* { dg-error "duplicate initialization" } */
