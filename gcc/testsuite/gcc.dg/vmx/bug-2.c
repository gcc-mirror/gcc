#include "harness.h"

static vector unsigned char value =
  { 0x7c, 0x12, 0x1, 0xd5,
    0xc3, 0x99, 0x21, 0xe2,
    0x12, 0x57, 0xde, 0x6b,
    0x39, 0x66, 0xa8, 0x87 };

void initn_c (int p1, int p2, signed char p3, int p4, double p5 ,
	      vector unsigned char p6, signed char p7)
{
  check(p1 == 3, "p1");
  check(p2 == 4, "p2");
  check(p3 == 5, "p3");
  check(p4 == 6, "p4");
  check(p5 == 1, "p5");
  check(vec_all_eq(p6, value), "p6");
  check(p7 == 7, "p7");
}
         
void test()
{
  initn_c (3, 4, 5, 6, 1, value, 7);
}
