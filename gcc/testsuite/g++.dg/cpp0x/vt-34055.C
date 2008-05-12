// { dg-options "-std=c++0x" }
// PR c++/34055
template<typename...> struct A;

template<typename...T> struct A<T*> // { dg-error "parameter packs|T" }
{
  void foo();
};

template<typename...T> void A<T*>::foo() {} // { dg-error "invalid declarator" }



template<typename...> struct B;

template<typename...T> struct B<T&> // { dg-error "parameter packs|T" }
{
  void foo();
};

template<typename...T> void B<T&>::foo() {} // { dg-error "invalid declarator" }


template<typename...> struct C;

template<typename...T> struct C<T()> // { dg-error "parameter packs|T" }
{
  void foo();
};

template<typename...T> void C<T()>::foo() {} // { dg-error "invalid declarator" }
