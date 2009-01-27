/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O2 -mfpu=neon -mfloat-abi=softfp" } */
/* Check that the arm_final_prescan_insn ccfsm code does not try to
 * conditionally execute NEON instructions.  */
#include <arm_neon.h>
#include <stdlib.h>

int __attribute__((noinline))
foo(uint32x2_t a, uint32_t *p, uint32_t *q)
{
  if (p != q)
    /* This vst1 instruction could be conditional, except that NEON
       instructions are never conditional in ARM mode.  */
    vst1_u32(p, a);
  return 0;
}

int
main()
{
    uint32x2_t v;
    uint32_t a[2] = {1, 42};
    v = vld1_u32(a);
    v = vadd_u32(v, v);
    foo(v, a, a);
    if (a[0] != 1 || a[1] != 42)
      abort();
    exit(0);
}
