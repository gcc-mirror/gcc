/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-additional-options "-O1 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-add-options riscv_v } */
/* { dg-final { check-function-bodies "**" "" } } */

#if 0 /* Using include files when using a multilib-relevant -march option is dicey */
#include <string.h>
#else
extern void *memcpy(void *__restrict dest, const void *__restrict src, __SIZE_TYPE__ n);
#endif

#define MIN_VECTOR_BYTES (__riscv_v_min_vlen / 8)

/* memcpy should be implemented using the cpymem pattern.
** f1:
XX	\.L\d+: # local label is ignored
**	vsetvli\s+[ta][0-7],a2,e8,m1,ta,ma
**	vle8\.v\s+v\d+,0\(a1\)
**	vse8\.v\s+v\d+,0\(a0\)
**	add\s+a1,a1,[ta][0-7]
**	add\s+a0,a0,[ta][0-7]
**	sub\s+a2,a2,[ta][0-7]
**	bne\s+a2,zero,\.L\d+
**	ret
*/

void f1 (void *a, void *b, __SIZE_TYPE__ l)
{
  memcpy (a, b, l);
}

/* We should still use cpymem even with slightly different types, as signed
   overflow is undefined.
** f2:
XX	\.L\d+: # local label is ignored
**	vsetvli\s+[ta][0-7],a2,e8,m1,ta,ma
**	vle8\.v\s+v\d+,0\(a1\)
**	vse8\.v\s+v\d+,0\(a0\)
**	add\s+a1,a1,[ta][0-7]
**	add\s+a0,a0,[ta][0-7]
**	sub\s+a2,a2,[ta][0-7]
**	bne\s+a2,zero,\.L\d+
**	ret
*/
void f2 (__INT32_TYPE__* a, __INT32_TYPE__* b, int l)
{
  memcpy (a, b, l);
}

/* If it is known that the pointer arguments to memcpy point
   to an aligned object, cpymem can use that alignment.
   Use extern here so that we get a known alignment, lest
   DATA_ALIGNMENT force us to make the scan pattern accomodate
   code for different alignments depending on word size.
** f3: { target { no-opts "-mrvv-vector-bits=zvl" } }
**  (
**  lui\s+[ta][0-7],%hi\(a_a\)
**  lui\s+[ta][0-7],%hi\(a_b\)
**  addi\s+[ta][0-7],[ta][0-7],%lo\(a_b\)
**  vsetivli\s+zero,4,e32,m1,ta,ma
**  |
**  lui\s+[ta][0-7],%hi\(a_a\)
**  lui\s+[ta][0-7],%hi\(a_b\)
**  li\s+[ta][0-7],\d+
**  addi\s+[ta][0-7],[ta][0-7],%lo\(a_b\)
**  vsetvli\s+zero,[ta][0-7],e32,m1,ta,ma
**  |
**  lla\s+[ta][0-7],a_b
**  vsetivli\s+zero,4,e32,m1,ta,ma
**  |
**  li\s+[ta][0-7],\d+
**  lla\s+[ta][0-7],a_b
**  vsetvli\s+zero,[ta][0-7],e32,m1,ta,ma
**  |
**  lla\s+[ta][0-7],a_b
**  li\s+[ta][0-7],32
**  vsetvli\s+zero,[ta][0-7],e32,m1,ta,ma
**  )
**  vle32.v\s+v\d+,0\([ta][0-7]\)
**  (
**  addi\s+[ta][0-7],[ta][0-7],%lo\(a_a\)
**  |
**  lla\s+[ta][0-7],a_a
**  )
**  vse32.v\s+v\d+,0\([ta][0-7]\)
**  ret
*/

/*
** f3: { target { any-opts "-mrvv-vector-bits=zvl" } }
**  (
**  lui\s+[ta][0-7],%hi\(a_a\)
**  lui\s+[ta][0-7],%hi\(a_b\)
**  addi\s+[ta][0-7],[ta][0-7],%lo\(a_a\)
**  addi\s+[ta][0-7],[ta][0-7],%lo\(a_b\)
**  |
**  lla\s+[ta][0-7],a_a
**  lla\s+[ta][0-7],a_b
**  )
**  vl1re32.v\s+v\d+,0\([ta][0-7]\)
**  vs1r.v\s+v\d+,0\([ta][0-7]\)
**  ret
*/

extern struct { __INT32_TYPE__ a[MIN_VECTOR_BYTES / 4]; } a_a, a_b;

void f3 ()
{
  memcpy (&a_a, &a_b, sizeof a_a);
}

/* { dg-final { scan-assembler-not {\m(tail|call)\s+memcpy\M} } } */
