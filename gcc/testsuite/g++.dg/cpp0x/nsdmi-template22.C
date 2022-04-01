// PR c++/102990
// { dg-do compile { target c++11 } }

struct knob_t {
  /* Let's create a FIX_TRUNC_EXPR.  */
  int value = 1.0;
};

struct Helpers {
  knob_t inputs;
};

template<class> void f(decltype(Helpers{}));
