template <typename T, bool=T::X> struct A
{
  int i;
};

template <typename T> struct B : A<T>
{
  using A<T>::i; // { dg-error "" } 
};

B<void> b; // { dg-error "" }
