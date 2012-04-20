#include "avx-check.h"
#include "args.h"

struct IntegerRegisters iregs;
struct FloatRegisters fregs;
unsigned int num_iregs, num_fregs;

struct m256_struct
{
  __m256 x;
};

struct m256_2_struct
{
  __m256 x1, x2;
};

/* Check that the struct is passed as the individual members in fregs.  */
void
check_struct_passing1 (struct m256_struct ms1 ATTRIBUTE_UNUSED,
		       struct m256_struct ms2 ATTRIBUTE_UNUSED,
		       struct m256_struct ms3 ATTRIBUTE_UNUSED,
		       struct m256_struct ms4 ATTRIBUTE_UNUSED,
		       struct m256_struct ms5 ATTRIBUTE_UNUSED,
		       struct m256_struct ms6 ATTRIBUTE_UNUSED,
		       struct m256_struct ms7 ATTRIBUTE_UNUSED,
		       struct m256_struct ms8 ATTRIBUTE_UNUSED)
{
  check_m256_arguments;
}

void
check_struct_passing2 (struct m256_2_struct ms ATTRIBUTE_UNUSED)
{
  /* Check the passing on the stack by comparing the address of the
     stack elements to the expected place on the stack.  */
  assert ((unsigned long)&ms.x1 == rsp+8);
  assert ((unsigned long)&ms.x2 == rsp+40);
}

static void
avx_test (void)
{
  struct m256_struct m256s [8];
  struct m256_2_struct m256_2s = { 
      { 48.394, 39.3, -397.9, 3484.9, -8.394, -93.3, 7.9, 84.94 },
      { -8.394, -3.3, -39.9, 34.9, 7.9, 84.94, -48.394, 39.3 }
  };
  int i;

  for (i = 0; i < 8; i++)
    m256s[i].x = (__m256){32+i, 0, i, 0, -i, 0, i - 12, i + 8};

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    (&fregs.ymm0)[i]._m256[0] = m256s[i].x;
  num_fregs = 8;
  WRAP_CALL (check_struct_passing1)(m256s[0], m256s[1], m256s[2], m256s[3],
				    m256s[4], m256s[5], m256s[6], m256s[7]);
  WRAP_CALL (check_struct_passing2)(m256_2s);
}
