#include "harness.h"

static vector unsigned int funny()
{
  vector unsigned int a;
  return vec_andc(vec_add(a,a),vec_add(a,a));
}

static void test()
{
  static vector unsigned int zero;
  check(vec_all_eq(funny(), zero), "funny");
}
