#include "bf16-check.h"
#include "defines.h"
#include "args.h"

struct FloatRegisters fregs;
struct IntegerRegisters iregs;
unsigned int num_fregs, num_iregs;

struct m128bf16_struct
{
  __m128bf16 x;
};

struct m128bf16_2_struct
{
  __m128bf16 x1, x2;
};

/* Check that the struct is passed as the individual members in fregs.  */
void
check_struct_passing1bf16 (struct m128bf16_struct ms1 ATTRIBUTE_UNUSED,
			   struct m128bf16_struct ms2 ATTRIBUTE_UNUSED,
			   struct m128bf16_struct ms3 ATTRIBUTE_UNUSED,
			   struct m128bf16_struct ms4 ATTRIBUTE_UNUSED,
			   struct m128bf16_struct ms5 ATTRIBUTE_UNUSED,
			   struct m128bf16_struct ms6 ATTRIBUTE_UNUSED,
			   struct m128bf16_struct ms7 ATTRIBUTE_UNUSED,
			   struct m128bf16_struct ms8 ATTRIBUTE_UNUSED)
{
  check_m128_arguments;
}

void
check_struct_passing2bf16 (struct m128bf16_2_struct ms ATTRIBUTE_UNUSED)
{
  /* Check the passing on the stack by comparing the address of the
     stack elements to the expected place on the stack.  */
  assert ((unsigned long)&ms.x1 == rsp+8);
  assert ((unsigned long)&ms.x2 == rsp+24);
}

volatile __bf16 bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8,
		bf9, bf10,bf11,bf12,bf13,bf14,bf15,bf16;

static void
do_test (void)
{
  struct m128bf16_struct m128bf16s [8];
  struct m128bf16_2_struct m128bf16_2s = { 
    { bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8 },
    { bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8 },
  };
  int i;

  for (i = 0; i < 8; i++)
    {
      m128bf16s[i].x = (__m128bf16) { bf1, bf2, bf3, bf4, bf5, bf6, bf7, bf8 };
    }

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    (&fregs.xmm0)[i]._m128bf16[0] = m128bf16s[i].x;
  num_fregs = 8;
  WRAP_CALL (check_struct_passing1bf16) (m128bf16s[0], m128bf16s[1], m128bf16s[2], m128bf16s[3],
					 m128bf16s[4], m128bf16s[5], m128bf16s[6], m128bf16s[7]);
  WRAP_CALL (check_struct_passing2bf16) (m128bf16_2s);
}
