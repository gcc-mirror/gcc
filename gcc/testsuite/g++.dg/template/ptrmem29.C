// PR c++/62659

struct D {
  typedef int (D::*cont_func)();
  template <cont_func> struct B;
  template <cont_func cont_f> void wait(B<cont_f> ***);

  int done();
  template <bool> void fix() { wait<&D::done>(0); }
};
