#include "harness.h"

extern vector pixel p16;
extern vector pixel const p16c;
extern vector pixel volatile p16v;
extern const vector pixel p16c_;
extern volatile vector pixel p16v_;

static void test()
{
  int i_p16 = vec_step(p16);
  int i_p16c = vec_step(p16c);
  int i_p16v = vec_step(p16v);
  int i_p16c_ = vec_step(p16c_);
  int i_p16v_ = vec_step(p16v_);
  check((i_p16 + i_p16c + i_p16v + i_p16c_ + i_p16v_) == 40, "vec_step");
}
