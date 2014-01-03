#include "avx512f-check.h"
#include "args.h"

struct IntegerRegisters iregs;
struct FloatRegisters fregs;
unsigned int num_iregs, num_fregs;

struct m512_struct
{
  __m512 x;
};

struct m512_2_struct
{
  __m512 x1, x2;
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
  /* Check the passing on the stack by comparing the address of the
     stack elements to the expected place on the stack.  */
  assert ((unsigned long)&ms1.x == rsp+8);
  assert ((unsigned long)&ms2.x == rsp+72);
  assert ((unsigned long)&ms3.x == rsp+136);
  assert ((unsigned long)&ms4.x == rsp+200);
  assert ((unsigned long)&ms5.x == rsp+264);
  assert ((unsigned long)&ms6.x == rsp+328);
  assert ((unsigned long)&ms7.x == rsp+392);
  assert ((unsigned long)&ms8.x == rsp+456);
}

void
check_struct_passing2 (struct m512_2_struct ms ATTRIBUTE_UNUSED)
{
  /* Check the passing on the stack by comparing the address of the
     stack elements to the expected place on the stack.  */
  assert ((unsigned long)&ms.x1 == rsp+8);
  assert ((unsigned long)&ms.x2 == rsp+72);
}

static void
avx512f_test (void)
{
  struct m512_struct m512s [8];
  struct m512_2_struct m512_2s = {
      { 48.394, 39.3, -397.9, 3484.9, -8.394, -93.3, 7.9, 84.94,
	48.3941, 39.31, -397.91, 3484.91, -8.3941, -93.31, 7.91, 84.941 },
      { -8.394, -3.3, -39.9, 34.9, 7.9, 84.94, -48.394, 39.3,
	-8.3942, -3.32, -39.92, 34.92, 7.92, 84.942, -48.3942, 39.32 }
  };
  int i;

  for (i = 0; i < 8; i++)
    m512s[i].x = (__m512){32+i, 0, i, 0, -i, 0, i - 12, i + 8,
			  32+i, 0, i, 0, -i, 0, i - 12, i + 8};

  clear_struct_registers;
  for (i = 0; i < 8; i++)
    (&fregs.zmm0)[i]._m512[0] = m512s[i].x;
  num_fregs = 8;
  WRAP_CALL (check_struct_passing1)(m512s[0], m512s[1], m512s[2], m512s[3],
				    m512s[4], m512s[5], m512s[6], m512s[7]);
  WRAP_CALL (check_struct_passing2)(m512_2s);
}
