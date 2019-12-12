// PR c++/88869
// { dg-do compile { target c++17 } }

template <typename> struct B;
template <> struct B<int> {
  template <typename T> struct C {
    T e;
    C (T f) : e(f) {}
  };
  void foo () { C c (42); }
};
