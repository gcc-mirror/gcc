// PR c++/124431
// { dg-additional-options "-fmodules" }
// { dg-module-cmi A }

export module A;

export template <int N> struct mat {
  int data[1] = { 123 };
  constexpr const int& operator[](int x) const { return data[x]; };
};

export template <int N> struct S {
  int field;
  enum e { a };
  enum class ce { ca };
};
