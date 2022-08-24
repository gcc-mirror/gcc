#include "bf16-ymm-check.h"
#include "args.h"

struct FloatRegisters fregs;
struct IntegerRegisters iregs;
unsigned int num_fregs, num_iregs;

union un1b
{
  __m256bf16 x;
  float f;
};

union un1bb
{
  __m256bf16 x;
  __bf16 f;
};

union un2b
{
  __m256bf16 x;
  double d;
};

union un3b
{
  __m256bf16 x;
  __m128 v;
};

union un4b
{
  __m256bf16 x;
  long double ld;
};

union un5b
{
  __m256bf16 x;
  int i;
};

void
check_union_passing1b (union un1b u1 ATTRIBUTE_UNUSED,
		       union un1b u2 ATTRIBUTE_UNUSED,
		       union un1b u3 ATTRIBUTE_UNUSED,
		       union un1b u4 ATTRIBUTE_UNUSED,
		       union un1b u5 ATTRIBUTE_UNUSED,
		       union un1b u6 ATTRIBUTE_UNUSED,
		       union un1b u7 ATTRIBUTE_UNUSED,
		       union un1b u8 ATTRIBUTE_UNUSED)
{
  check_m256_arguments;
}

void
check_union_passing1bb (union un1bb u1 ATTRIBUTE_UNUSED,
		        union un1bb u2 ATTRIBUTE_UNUSED,
		        union un1bb u3 ATTRIBUTE_UNUSED,
		        union un1bb u4 ATTRIBUTE_UNUSED,
		        union un1bb u5 ATTRIBUTE_UNUSED,
		        union un1bb u6 ATTRIBUTE_UNUSED,
		        union un1bb u7 ATTRIBUTE_UNUSED,
		        union un1bb u8 ATTRIBUTE_UNUSED)
{
  check_m256_arguments;
}

void
check_union_passing2b (union un2b u1 ATTRIBUTE_UNUSED,
		       union un2b u2 ATTRIBUTE_UNUSED,
		       union un2b u3 ATTRIBUTE_UNUSED,
		       union un2b u4 ATTRIBUTE_UNUSED,
		       union un2b u5 ATTRIBUTE_UNUSED,
		       union un2b u6 ATTRIBUTE_UNUSED,
		       union un2b u7 ATTRIBUTE_UNUSED,
		       union un2b u8 ATTRIBUTE_UNUSED)
{
  check_m256_arguments;
}

void
check_union_passing3b (union un3b u1 ATTRIBUTE_UNUSED,
		       union un3b u2 ATTRIBUTE_UNUSED,
		       union un3b u3 ATTRIBUTE_UNUSED,
		       union un3b u4 ATTRIBUTE_UNUSED,
		       union un3b u5 ATTRIBUTE_UNUSED,
		       union un3b u6 ATTRIBUTE_UNUSED,
		       union un3b u7 ATTRIBUTE_UNUSED,
		       union un3b u8 ATTRIBUTE_UNUSED)
{
  check_m256_arguments;
}

void
check_union_passing4b (union un4b u ATTRIBUTE_UNUSED)
{
   /* Check the passing on the stack by comparing the address of the
      stack elements to the expected place on the stack.  */
  assert ((unsigned long)&u.x == rsp+8);
  assert ((unsigned long)&u.ld == rsp+8);
}

void
check_union_passing5b (union un5b u ATTRIBUTE_UNUSED)
{
   /* Check the passing on the stack by comparing the address of the
      stack elements to the expected place on the stack.  */
  assert ((unsigned long)&u.x == rsp+8);
  assert ((unsigned long)&u.i == rsp+8);
}

#define check_union_passing1b WRAP_CALL(check_union_passing1b)
#define check_union_passing1bb WRAP_CALL(check_union_passing1bb)
#define check_union_passing2b WRAP_CALL(check_union_passing2b)
#define check_union_passing3b WRAP_CALL(check_union_passing3b)
#define check_union_passing4b WRAP_CALL(check_union_passing4b)
#define check_union_passing5b WRAP_CALL(check_union_passing5b)

static void
do_test (void)
{
  union un1b u1b[8];
  union un1bb u1bb[8];
  union un2b u2b[8];
  union un3b u3b[8];
  union un4b u4b;
  union un5b u5b;
  int i;
  __bf16 bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8,
	 bf9, bf10,bf11,bf12,bf13,bf14,bf15,bf16;

  for (i = 0; i < 8; i++)
    {
      u1b[i].x = (__m256bf16) { bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8,
				bf9, bf10,bf11,bf12,bf13,bf14,bf15,bf16 };
    }

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    (&fregs.ymm0)[i]._m256bf16[0] = u1b[i].x;
  num_fregs = 8;
  check_union_passing1b (u1b[0], u1b[1], u1b[2], u1b[3],
		         u1b[4], u1b[5], u1b[6], u1b[7]);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    {
      u1bb[i].x = u1b[i].x;
      (&fregs.ymm0)[i]._m256bf16[0] = u1bb[i].x;
    }
  num_fregs = 8;
  check_union_passing1bb (u1bb[0], u1bb[1], u1bb[2], u1bb[3],
		          u1bb[4], u1bb[5], u1bb[6], u1bb[7]);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    {
      u2b[i].x = u1b[i].x;
      (&fregs.ymm0)[i]._m256bf16[0] = u2b[i].x;
    }
  num_fregs = 8;
  check_union_passing2b (u2b[0], u2b[1], u2b[2], u2b[3],
		         u2b[4], u2b[5], u2b[6], u2b[7]);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    {
      u3b[i].x = u1b[i].x;
      (&fregs.ymm0)[i]._m256bf16[0] = u3b[i].x;
    }
  num_fregs = 8;
  check_union_passing3b (u3b[0], u3b[1], u3b[2], u3b[3],
		         u3b[4], u3b[5], u3b[6], u3b[7]);

  check_union_passing4b (u4b);
  check_union_passing5b (u5b);
}
