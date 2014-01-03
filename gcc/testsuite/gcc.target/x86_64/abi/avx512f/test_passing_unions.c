#include "avx512f-check.h"
#include "args.h"

struct IntegerRegisters iregs;
struct FloatRegisters fregs;
unsigned int num_iregs, num_fregs;

union un1
{
  __m512 x;
  float f;
};

union un2
{
  __m512 x;
  double d;
};

union un3
{
  __m512 x;
  __m128 v;
};

union un4
{
  __m512 x;
  long double ld;
};

union un5
{
  __m512 x;
  int i;
};

union un6
{
  __m512 x;
  __m256 v;
};


void
check_union_passing1(union un1 u1 ATTRIBUTE_UNUSED,
		     union un1 u2 ATTRIBUTE_UNUSED,
		     union un1 u3 ATTRIBUTE_UNUSED,
		     union un1 u4 ATTRIBUTE_UNUSED,
		     union un1 u5 ATTRIBUTE_UNUSED,
		     union un1 u6 ATTRIBUTE_UNUSED,
		     union un1 u7 ATTRIBUTE_UNUSED,
		     union un1 u8 ATTRIBUTE_UNUSED)
{
   /* Check the passing on the stack by comparing the address of the
      stack elements to the expected place on the stack.  */
  assert ((unsigned long)&u1.x == rsp+8);
  assert ((unsigned long)&u1.f == rsp+8);
  assert ((unsigned long)&u2.x == rsp+72);
  assert ((unsigned long)&u2.f == rsp+72);
  assert ((unsigned long)&u3.x == rsp+136);
  assert ((unsigned long)&u3.f == rsp+136);
  assert ((unsigned long)&u4.x == rsp+200);
  assert ((unsigned long)&u4.f == rsp+200);
  assert ((unsigned long)&u5.x == rsp+264);
  assert ((unsigned long)&u5.f == rsp+264);
  assert ((unsigned long)&u6.x == rsp+328);
  assert ((unsigned long)&u6.f == rsp+328);
  assert ((unsigned long)&u7.x == rsp+392);
  assert ((unsigned long)&u7.f == rsp+392);
  assert ((unsigned long)&u8.x == rsp+456);
  assert ((unsigned long)&u8.f == rsp+456);
}

void
check_union_passing2(union un2 u1 ATTRIBUTE_UNUSED,
		     union un2 u2 ATTRIBUTE_UNUSED,
		     union un2 u3 ATTRIBUTE_UNUSED,
		     union un2 u4 ATTRIBUTE_UNUSED,
		     union un2 u5 ATTRIBUTE_UNUSED,
		     union un2 u6 ATTRIBUTE_UNUSED,
		     union un2 u7 ATTRIBUTE_UNUSED,
		     union un2 u8 ATTRIBUTE_UNUSED)
{
   /* Check the passing on the stack by comparing the address of the
      stack elements to the expected place on the stack.  */
  assert ((unsigned long)&u1.x == rsp+8);
  assert ((unsigned long)&u1.d == rsp+8);
  assert ((unsigned long)&u2.x == rsp+72);
  assert ((unsigned long)&u2.d == rsp+72);
  assert ((unsigned long)&u3.x == rsp+136);
  assert ((unsigned long)&u3.d == rsp+136);
  assert ((unsigned long)&u4.x == rsp+200);
  assert ((unsigned long)&u4.d == rsp+200);
  assert ((unsigned long)&u5.x == rsp+264);
  assert ((unsigned long)&u5.d == rsp+264);
  assert ((unsigned long)&u6.x == rsp+328);
  assert ((unsigned long)&u6.d == rsp+328);
  assert ((unsigned long)&u7.x == rsp+392);
  assert ((unsigned long)&u7.d == rsp+392);
  assert ((unsigned long)&u8.x == rsp+456);
  assert ((unsigned long)&u8.d == rsp+456);
}

void
check_union_passing3(union un3 u1 ATTRIBUTE_UNUSED,
		     union un3 u2 ATTRIBUTE_UNUSED,
		     union un3 u3 ATTRIBUTE_UNUSED,
		     union un3 u4 ATTRIBUTE_UNUSED,
		     union un3 u5 ATTRIBUTE_UNUSED,
		     union un3 u6 ATTRIBUTE_UNUSED,
		     union un3 u7 ATTRIBUTE_UNUSED,
		     union un3 u8 ATTRIBUTE_UNUSED)
{
   /* Check the passing on the stack by comparing the address of the
      stack elements to the expected place on the stack.  */
  assert ((unsigned long)&u1.x == rsp+8);
  assert ((unsigned long)&u1.v == rsp+8);
  assert ((unsigned long)&u2.x == rsp+72);
  assert ((unsigned long)&u2.v == rsp+72);
  assert ((unsigned long)&u3.x == rsp+136);
  assert ((unsigned long)&u3.v == rsp+136);
  assert ((unsigned long)&u4.x == rsp+200);
  assert ((unsigned long)&u4.v == rsp+200);
  assert ((unsigned long)&u5.x == rsp+264);
  assert ((unsigned long)&u5.v == rsp+264);
  assert ((unsigned long)&u6.x == rsp+328);
  assert ((unsigned long)&u6.v == rsp+328);
  assert ((unsigned long)&u7.x == rsp+392);
  assert ((unsigned long)&u7.v == rsp+392);
  assert ((unsigned long)&u8.x == rsp+456);
  assert ((unsigned long)&u8.v == rsp+456);
}

void
check_union_passing4(union un4 u ATTRIBUTE_UNUSED)
{
   /* Check the passing on the stack by comparing the address of the
      stack elements to the expected place on the stack.  */
  assert ((unsigned long)&u.x == rsp+8);
  assert ((unsigned long)&u.ld == rsp+8);
}

void
check_union_passing5(union un5 u ATTRIBUTE_UNUSED)
{
   /* Check the passing on the stack by comparing the address of the
      stack elements to the expected place on the stack.  */
  assert ((unsigned long)&u.x == rsp+8);
  assert ((unsigned long)&u.i == rsp+8);
}

void
check_union_passing6(union un6 u1 ATTRIBUTE_UNUSED,
		     union un6 u2 ATTRIBUTE_UNUSED,
		     union un6 u3 ATTRIBUTE_UNUSED,
		     union un6 u4 ATTRIBUTE_UNUSED,
		     union un6 u5 ATTRIBUTE_UNUSED,
		     union un6 u6 ATTRIBUTE_UNUSED,
		     union un6 u7 ATTRIBUTE_UNUSED,
		     union un6 u8 ATTRIBUTE_UNUSED)
{
  assert ((unsigned long)&u1.x == rsp+8);
  assert ((unsigned long)&u1.v == rsp+8);
  assert ((unsigned long)&u2.x == rsp+72);
  assert ((unsigned long)&u2.v == rsp+72);
  assert ((unsigned long)&u3.x == rsp+136);
  assert ((unsigned long)&u3.v == rsp+136);
  assert ((unsigned long)&u4.x == rsp+200);
  assert ((unsigned long)&u4.v == rsp+200);
  assert ((unsigned long)&u5.x == rsp+264);
  assert ((unsigned long)&u5.v == rsp+264);
  assert ((unsigned long)&u6.x == rsp+328);
  assert ((unsigned long)&u6.v == rsp+328);
  assert ((unsigned long)&u7.x == rsp+392);
  assert ((unsigned long)&u7.v == rsp+392);
  assert ((unsigned long)&u8.x == rsp+456);
  assert ((unsigned long)&u8.v == rsp+456);
}

#define check_union_passing1 WRAP_CALL(check_union_passing1)
#define check_union_passing2 WRAP_CALL(check_union_passing2)
#define check_union_passing3 WRAP_CALL(check_union_passing3)
#define check_union_passing4 WRAP_CALL(check_union_passing4)
#define check_union_passing5 WRAP_CALL(check_union_passing5)
#define check_union_passing6 WRAP_CALL(check_union_passing6)

static void
avx512f_test (void)
{
  union un1 u1[8];
  union un2 u2[8];
  union un3 u3[8];
  union un4 u4;
  union un5 u5;
  union un6 u6[8];
  int i;

  for (i = 0; i < 8; i++)
    u1[i].x = (__m512){32+i, 0, i, 0, -i, 0, i - 12, i + 8,
		       32+i, 0, i, 0, -i, 0, i - 12, i + 8};

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    (&fregs.zmm0)[i]._m512[0] = u1[i].x;
  num_fregs = 8;
  check_union_passing1(u1[0], u1[1], u1[2], u1[3],
		       u1[4], u1[5], u1[6], u1[7]);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    {
      u2[i].x = u1[i].x;
      (&fregs.zmm0)[i]._m512[0] = u2[i].x;
    }
  num_fregs = 8;
  check_union_passing2(u2[0], u2[1], u2[2], u2[3],
		       u2[4], u2[5], u2[6], u2[7]);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    {
      u3[i].x = u1[i].x;
      (&fregs.zmm0)[i]._m512[0] = u3[i].x;
    }
  num_fregs = 8;
  check_union_passing3(u3[0], u3[1], u3[2], u3[3],
		       u3[4], u3[5], u3[6], u3[7]);

  check_union_passing4(u4);
  check_union_passing5(u5);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    {
      u6[i].x = u1[i].x;
      (&fregs.zmm0)[i]._m512[0] = u6[i].x;
    }
  num_fregs = 8;
  check_union_passing6(u6[0], u6[1], u6[2], u6[3],
		       u6[4], u6[5], u6[6], u6[7]);
}
