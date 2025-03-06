// PR c++/119123
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -g" }

struct S {
  template <typename> void foo (int = [] {}) const;
};
struct T {
  static void bar (const S &);
};
