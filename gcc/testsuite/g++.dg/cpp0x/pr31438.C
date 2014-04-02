// { dg-do compile { target c++11 } }

template<typename> struct A;
template<typename T, typename... U> struct A<T(U)> // { dg-error "parameter packs|U" }
{
 template<typename X> A(X);
};

A<void(int)> a(0); // { dg-error "incomplete type" }
