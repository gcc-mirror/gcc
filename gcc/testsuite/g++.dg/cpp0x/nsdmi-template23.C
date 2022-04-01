// PR c++/102990
// { dg-do compile { target c++11 } }

struct knob_t {
  /* Let's create a FLOAT_EXPR.  */
  double value = 1UL;
};

struct Helpers {
  knob_t inputs;
};

template<class> void f(decltype(Helpers{}));
