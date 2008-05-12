// { dg-options "-std=c++0x" }
template<typename...> struct A;

template<typename T, typename... U> struct A<T, U> // { dg-error "parameter packs|U" }
{
  template<typename> struct B;

  template<typename X> struct B<X*> {};
};
