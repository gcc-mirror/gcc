// PR c++/61636
// { dg-do compile { target c++14 } }

// ICE because we figure this capture too late.

struct Base
{
  void Bar (int);
};

struct A : Base {
  void b ();
  void Foo (int);
  using Base::Bar;
  template <typename T> void Baz (T);
};

void A::b() {

  auto lam = [&](auto asdf) { Foo (asdf); };

  lam (0);

  auto lam1 = [&](auto asdf) { Bar (asdf); };

  lam1 (0);

  auto lam2 = [&](auto asdf) { Baz (asdf); };

  lam2 (0);
}
