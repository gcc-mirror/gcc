// PR c++/61636
// { dg-do compile { target c++14 } }
// permissiveness doesn't make this permitted
// { dg-additional-options "-fpermissive" }

// ICE because we attempt to use dependent Foo during error recovery
// and die with an unexpected this capture need.

template <typename T> struct Base
{
  void Foo (int);
};

template <typename T> struct A : Base<T> {
  void b ();
};

template <typename T> void A<T>::b() {

  auto lam = [&](auto asdf) { Foo (asdf); }; // { dg-error "not declared" }

  lam (T(0));
}

template void A<int>::b ();
