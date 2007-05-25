// { dg-options "-std=gnu++0x" }

template<typename> struct A; // { dg-error "candidates" }
template<typename T, typename... U> struct A<T(U)> // { dg-error "parameter packs|U" }
{ // { dg-error "parameter packs|U" }
 template<typename X> A(X); // { dg-error "parameter packs|U" }
};

A<void(int)> a(0); // { dg-error "no matching" }
