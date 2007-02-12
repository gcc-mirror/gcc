/* PR c++/14622. The invalid explicit instantiation was not reported. */
/* { dg-do "compile" } */
template<class T>
class A
{
  static T a;
};

template<class T>
T A<T>::a;

struct B {};

template B A<int>::a; /* { dg-error "does not match declared type" } */
template float A<float>::a;
