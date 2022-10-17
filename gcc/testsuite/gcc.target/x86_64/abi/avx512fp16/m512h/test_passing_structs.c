#include "avx512fp16-zmm-check.h"
#include "args.h"

struct IntegerRegisters iregs;
struct FloatRegisters fregs;
unsigned int num_iregs, num_fregs;

struct m512_struct
{
  __m512 x;
};

struct m512h_struct
{
  __m512h x;
};

struct m512_2_struct
{
  __m512 x1, x2;
};

struct m512h_2_struct
{
  __m512h x1, x2;
};

/* Check that the struct is passed as the individual members in fregs.  */
void
check_struct_passing1 (struct m512_struct ms1 ATTRIBUTE_UNUSED,
		       struct m512_struct ms2 ATTRIBUTE_UNUSED,
		       struct m512_struct ms3 ATTRIBUTE_UNUSED,
		       struct m512_struct ms4 ATTRIBUTE_UNUSED,
		       struct m512_struct ms5 ATTRIBUTE_UNUSED,
		       struct m512_struct ms6 ATTRIBUTE_UNUSED,
		       struct m512_struct ms7 ATTRIBUTE_UNUSED,
		       struct m512_struct ms8 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m512_arguments;
}

void
check_struct_passing1h (struct m512h_struct ms1 ATTRIBUTE_UNUSED,
			struct m512h_struct ms2 ATTRIBUTE_UNUSED,
			struct m512h_struct ms3 ATTRIBUTE_UNUSED,
			struct m512h_struct ms4 ATTRIBUTE_UNUSED,
			struct m512h_struct ms5 ATTRIBUTE_UNUSED,
			struct m512h_struct ms6 ATTRIBUTE_UNUSED,
			struct m512h_struct ms7 ATTRIBUTE_UNUSED,
			struct m512h_struct ms8 ATTRIBUTE_UNUSED)
{
  /* Check register contents.  */
  check_m512_arguments;
}

void
check_struct_passing2 (struct m512_2_struct ms ATTRIBUTE_UNUSED)
{
  /* Check the passing on the stack by comparing the address of the
     stack elements to the expected place on the stack.  */
  assert ((unsigned long)&ms.x1 == rsp+8);
  assert ((unsigned long)&ms.x2 == rsp+72);
}

void
check_struct_passing2h (struct m512h_2_struct ms ATTRIBUTE_UNUSED)
{
  /* Check the passing on the stack by comparing the address of the
     stack elements to the expected place on the stack.  */
  assert ((unsigned long)&ms.x1 == rsp+8);
  assert ((unsigned long)&ms.x2 == rsp+72);
}

static void
do_test (void)
{
  struct m512_struct m512s [8];
  struct m512h_struct m512hs [8];
  struct m512_2_struct m512_2s = {
      { 48.394, 39.3, -397.9, 3484.9, -8.394, -93.3, 7.9, 84.94,
	48.3941, 39.31, -397.91, 3484.91, -8.3941, -93.31, 7.91, 84.941 },
      { -8.394, -3.3, -39.9, 34.9, 7.9, 84.94, -48.394, 39.3,
	-8.3942, -3.32, -39.92, 34.92, 7.92, 84.942, -48.3942, 39.32 }
  };
  struct m512h_2_struct m512h_2s = {
      { 58.395f16, 39.3f16, -397.9f16, 3585.9f16, -8.395f16, -93.3f16, 7.9f16, 85.95f16,
        58.395f16, 39.3f16, -397.9f16, 3585.9f16, -8.395f16, -93.3f16, 7.9f16, 85.95f16,
        58.395f16, 39.3f16, -397.9f16, 3585.9f16, -8.395f16, -93.3f16, 7.9f16, 85.95f16,
	58.3951f16, 39.31f16, -397.91f16, 3585.91f16, -8.3951f16, -93.31f16, 7.91f16, 85.951f16},
      { 67.396f16, 39.3f16, -397.9f16, 3676.9f16, -7.396f16, -93.3f16, 7.9f16, 76.96f16,
        67.396f16, 39.3f16, -397.9f16, 3676.9f16, -7.396f16, -93.3f16, 7.9f16, 76.96f16,
        67.396f16, 39.3f16, -397.9f16, 3676.9f16, -7.396f16, -93.3f16, 7.9f16, 76.96f16,
	67.3961f16, 39.31f16, -397.91f16, 3676.91f16, -7.3961f16, -93.31f16, 7.91f16, 76.961f16},
  };
  int i;

  for (i = 0; i < 8; i++)
    {
      m512s[i].x = (__m512){32+i, 0, i, 0, -i, 0, i - 12, i + 8,
			    32+i, 0, i, 0, -i, 0, i - 12, i + 8};
      m512hs[i].x = (__m512h){33+i, 1, i, 2, -i, 0, i - 15, i + 9,
			      34+i, 1, i, 2, -i, 0, i - 15, i + 9,
			      35+i, 1, i, 2, -i, 0, i - 15, i + 9,
			      36+i, 1, i, 2, -i, 0, i - 15, i + 9};
    }

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    (&fregs.zmm0)[i]._m512[0] = m512s[i].x;
  num_fregs = 8;
  WRAP_CALL (check_struct_passing1)(m512s[0], m512s[1], m512s[2], m512s[3],
				    m512s[4], m512s[5], m512s[6], m512s[7]);
  WRAP_CALL (check_struct_passing2)(m512_2s);

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    (&fregs.zmm0)[i]._m512h[0] = m512hs[i].x;
  num_fregs = 8;
  WRAP_CALL (check_struct_passing1h)(m512hs[0], m512hs[1], m512hs[2], m512hs[3],
				    m512hs[4], m512hs[5], m512hs[6], m512hs[7]);
  WRAP_CALL (check_struct_passing2h)(m512h_2s);
}
