// PR c++/89387
// { dg-do compile { target c++11 } }

template <template <typename, typename> class T>
struct S {
  using A = int;
  using B = T<unsigned, A>;
  using B::foo;
  void bar () { [&] { foo (); }; }
  void foo ();
};
