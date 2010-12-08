// { dg-options "-std=gnu++0x" }
template <typename... T> struct A // { dg-error "candidates|A" }
{
  A(T* p) {  // { dg-error "parameter packs|T" }
   (A<T...>*)(p); 
  }
};

A<int> a(0); // { dg-error "no matching" }
// { dg-message "candidate" "candidate note" { target *-*-* } 9 }
