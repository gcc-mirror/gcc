#include "avx512fp16-zmm-check.h"
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

union un1h
{
  __m512 x;
  _Float16 f;
};

union un1hf
{
  __m512h x;
  float f;
};

union un1hh
{
  __m512h x;
  _Float16 f;
};

union un2h
{
  __m512h x;
  double d;
};

union un3h
{
  __m512h x;
  __m128 v;
};

union un4h
{
  __m512h x;
  long double ld;
};

union un5h
{
  __m512h x;
  int i;
};

union un6h
{
  __m512h x;
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
  /* Check register contents.  */
  check_m512_arguments;
}

void
check_union_passing1h(union un1h u1 ATTRIBUTE_UNUSED,
		      union un1h u2 ATTRIBUTE_UNUSED,
		      union un1h u3 ATTRIBUTE_UNUSED,
		      union un1h u4 ATTRIBUTE_UNUSED,
		      union un1h u5 ATTRIBUTE_UNUSED,
		      union un1h u6 ATTRIBUTE_UNUSED,
		      union un1h u7 ATTRIBUTE_UNUSED,
		      union un1h u8 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m512_arguments;
}

void
check_union_passing1hf(union un1hf u1 ATTRIBUTE_UNUSED,
		       union un1hf u2 ATTRIBUTE_UNUSED,
		       union un1hf u3 ATTRIBUTE_UNUSED,
		       union un1hf u4 ATTRIBUTE_UNUSED,
		       union un1hf u5 ATTRIBUTE_UNUSED,
		       union un1hf u6 ATTRIBUTE_UNUSED,
		       union un1hf u7 ATTRIBUTE_UNUSED,
		       union un1hf u8 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m512_arguments;
}

void
check_union_passing1hh(union un1hh u1 ATTRIBUTE_UNUSED,
		       union un1hh u2 ATTRIBUTE_UNUSED,
		       union un1hh u3 ATTRIBUTE_UNUSED,
		       union un1hh u4 ATTRIBUTE_UNUSED,
		       union un1hh u5 ATTRIBUTE_UNUSED,
		       union un1hh u6 ATTRIBUTE_UNUSED,
		       union un1hh u7 ATTRIBUTE_UNUSED,
		       union un1hh u8 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m512_arguments;
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
  /* Check register contents.  */
  check_m512_arguments;
}

void
check_union_passing2h(union un2h u1 ATTRIBUTE_UNUSED,
		      union un2h u2 ATTRIBUTE_UNUSED,
		      union un2h u3 ATTRIBUTE_UNUSED,
		      union un2h u4 ATTRIBUTE_UNUSED,
		      union un2h u5 ATTRIBUTE_UNUSED,
		      union un2h u6 ATTRIBUTE_UNUSED,
		      union un2h u7 ATTRIBUTE_UNUSED,
		      union un2h u8 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m512_arguments;
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
  /* Check register contents.  */
  check_m512_arguments;
}

void
check_union_passing3h(union un3h u1 ATTRIBUTE_UNUSED,
		      union un3h u2 ATTRIBUTE_UNUSED,
		      union un3h u3 ATTRIBUTE_UNUSED,
		      union un3h u4 ATTRIBUTE_UNUSED,
		      union un3h u5 ATTRIBUTE_UNUSED,
		      union un3h u6 ATTRIBUTE_UNUSED,
		      union un3h u7 ATTRIBUTE_UNUSED,
		      union un3h u8 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m512_arguments;
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
check_union_passing4h(union un4h u ATTRIBUTE_UNUSED)
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
check_union_passing5h(union un5h u ATTRIBUTE_UNUSED)
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
  /* Check register contents.  */
  check_m512_arguments;
}

void
check_union_passing6h(union un6h u1 ATTRIBUTE_UNUSED,
		      union un6h u2 ATTRIBUTE_UNUSED,
		      union un6h u3 ATTRIBUTE_UNUSED,
		      union un6h u4 ATTRIBUTE_UNUSED,
		      union un6h u5 ATTRIBUTE_UNUSED,
		      union un6h u6 ATTRIBUTE_UNUSED,
		      union un6h u7 ATTRIBUTE_UNUSED,
		      union un6h u8 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m512_arguments;
}

#define check_union_passing1 WRAP_CALL(check_union_passing1)
#define check_union_passing2 WRAP_CALL(check_union_passing2)
#define check_union_passing3 WRAP_CALL(check_union_passing3)
#define check_union_passing4 WRAP_CALL(check_union_passing4)
#define check_union_passing5 WRAP_CALL(check_union_passing5)
#define check_union_passing6 WRAP_CALL(check_union_passing6)

#define check_union_passing1h WRAP_CALL(check_union_passing1h)
#define check_union_passing1hf WRAP_CALL(check_union_passing1hf)
#define check_union_passing1hh WRAP_CALL(check_union_passing1hh)
#define check_union_passing2h WRAP_CALL(check_union_passing2h)
#define check_union_passing3h WRAP_CALL(check_union_passing3h)
#define check_union_passing4h WRAP_CALL(check_union_passing4h)
#define check_union_passing5h WRAP_CALL(check_union_passing5h)
#define check_union_passing6h WRAP_CALL(check_union_passing6h)


static void
do_test (void)
{
  union un1 u1[8];
  union un2 u2[8];
  union un3 u3[8];
  union un4 u4;
  union un5 u5;
  union un6 u6[8];
  union un1h u1h[8];
  union un1hf u1hf[8];
  union un1hh u1hh[8];
  union un2h u2h[8];
  union un3h u3h[8];
  union un4h u4h;
  union un5h u5h;
  union un6h u6h[8];
   int i;

  for (i = 0; i < 8; i++)
    {
      u1[i].x = (__m512){32+i, 0, i, 0, -i, 0, i - 12, i + 8,
	                 32+i, 0, i, 0, -i, 0, i - 12, i + 8};

      u1hf[i].x =  (__m512h){ 33+i, 1, i, 2, -i, 0, i - 15, i + 9,
                              34+i, 1, i, 2, -i, 0, i - 15, i + 9,
                              35+i, 1, i, 2, -i, 0, i - 15, i + 9,
                              36+i, 1, i, 2, -i, 0, i - 15, i + 9};
    }

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    (&fregs.zmm0)[i]._m512[0] = u1[i].x;
  num_fregs = 8;
  check_union_passing1(u1[0], u1[1], u1[2], u1[3],
		       u1[4], u1[5], u1[6], u1[7]);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    {
      u1h[i].x = u1[i].x;
      (&fregs.zmm0)[i]._m512[0] = u1h[i].x;
    }
  num_fregs = 8;
  check_union_passing1h(u1h[0], u1h[1], u1h[2], u1h[3],
		        u1h[4], u1h[5], u1h[6], u1h[7]);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    (&fregs.zmm0)[i]._m512h[0] = u1hf[i].x;
  num_fregs = 8;
  check_union_passing1hf(u1hf[0], u1hf[1], u1hf[2], u1hf[3],
		         u1hf[4], u1hf[5], u1hf[6], u1hf[7]);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    {
      u1hh[i].x = u1hf[i].x;
      (&fregs.zmm0)[i]._m512h[0] = u1hh[i].x;
    }
  num_fregs = 8;
  check_union_passing1hh(u1hh[0], u1hh[1], u1hh[2], u1hh[3],
		         u1hh[4], u1hh[5], u1hh[6], u1hh[7]);

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
      u2h[i].x = u1hf[i].x;
      (&fregs.zmm0)[i]._m512h[0] = u2h[i].x;
    }
  num_fregs = 8;
  check_union_passing2h(u2h[0], u2h[1], u2h[2], u2h[3],
		        u2h[4], u2h[5], u2h[6], u2h[7]);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    {
      u3[i].x = u1[i].x;
      (&fregs.zmm0)[i]._m512[0] = u3[i].x;
    }
  num_fregs = 8;
  check_union_passing3(u3[0], u3[1], u3[2], u3[3],
		       u3[4], u3[5], u3[6], u3[7]);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    {
      u3h[i].x = u1hf[i].x;
      (&fregs.zmm0)[i]._m512h[0] = u3h[i].x;
    }
  num_fregs = 8;
  check_union_passing3h(u3h[0], u3h[1], u3h[2], u3h[3],
		        u3h[4], u3h[5], u3h[6], u3h[7]);

  check_union_passing4(u4);
  check_union_passing5(u5);

  check_union_passing4h(u4h);
  check_union_passing5h(u5h);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    {
      u6[i].x = u1[i].x;
      (&fregs.zmm0)[i]._m512[0] = u6[i].x;
    }
  num_fregs = 8;
  check_union_passing6(u6[0], u6[1], u6[2], u6[3],
		       u6[4], u6[5], u6[6], u6[7]);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    {
      u6h[i].x = u1hf[i].x;
      (&fregs.zmm0)[i]._m512h[0] = u6h[i].x;
    }
  num_fregs = 8;
  check_union_passing6h(u6h[0], u6h[1], u6h[2], u6h[3],
		        u6h[4], u6h[5], u6h[6], u6h[7]);
}
