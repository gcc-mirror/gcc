// { dg-options "-std=gnu++11" }
template <typename... T> struct A // { dg-message "candidates|A" }
{
  A(T* p) {  // { dg-error "parameter packs|T" }
   (A<T...>*)(p); 
  }
};

A<int> a(0); // { dg-error "no matching" }
// { dg-message "candidate" "candidate note" { target *-*-* } 9 }
