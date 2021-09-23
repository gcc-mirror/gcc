// PR c++/88325
// { dg-do compile { target c++20 } }

template<typename> struct A
{
  template<typename> A ();
};

template<typename T>
template<typename U>
A<T>::A<U> () // { dg-error "" }
{
}

template<typename> struct B
{
  template<typename> int foo (int);
};

template<typename T>
template<typename U>
B<T>::foo<int>(int) // { dg-error "" }
{
  return 1;
}
