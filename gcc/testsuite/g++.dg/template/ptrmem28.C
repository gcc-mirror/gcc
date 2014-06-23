// PR c++/61488

struct A {
  typedef int (A::*cont_func)();
  template <A::cont_func> void wait(int);
  int notify();

  void fix() { wait<&A::notify>(0); } // OK
  template <int> void repair() { wait<&A::notify>(0); }
};
