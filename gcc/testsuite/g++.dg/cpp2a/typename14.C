// PR c++/88325
// { dg-do compile { target c++2a } }

template<typename> struct A
{
  template<typename> A ();
};

template<typename T>
template<typename U>
A<T>::A<U> () // { dg-error "partial specialization" }
{
}

template<typename> struct B
{
  template<typename> int foo (int);
};

template<typename T>
template<typename U>
B<T>::foo<int>(int) // { dg-error "partial specialization|declaration" }
{
  return 1;
}
