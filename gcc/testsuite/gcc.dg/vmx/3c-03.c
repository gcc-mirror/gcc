#include "harness.h"

vector unsigned char u8;

static void f(int i, void *p)
{
  u8 = vec_ld(i, (unsigned char *)p);
}

static void test()
{
  static vector unsigned int value = {1,-2,3,-4};
  static vector unsigned int buffer[2];
  buffer[1] = value;
  f(37,(void *)(-37+(char*)(buffer+1)));
  check(vec_all_eq((vector unsigned int) u8, value), "u8");
}
