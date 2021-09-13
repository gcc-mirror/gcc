// PR c++/99120
// { dg-options "-Wshadow" }

struct S {
  void X();

  template<typename T>
  void fn () {
    enum { X };
  }
};
