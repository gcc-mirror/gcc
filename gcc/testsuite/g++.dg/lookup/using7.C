template <typename T, bool=T::X> struct A
{
  int i;
};

template <typename T> struct B : A<T> // { dg-error "incomplete" }
{
  using A<T>::i; // { dg-error "incomplete" "incomplete" } 
};

B<void> b; // { dg-message "required" }
