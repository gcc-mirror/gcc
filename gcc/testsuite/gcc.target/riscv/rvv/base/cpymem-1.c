/* { dg-do compile } */
/* { dg-additional-options "-O1 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-add-options riscv_v } */
/* { dg-final { check-function-bodies "**" "" } } */

#if 0 /* Using include files when using a multilib-relevant -march option is dicey */
#include <string.h>
#else
extern void *memcpy(void *__restrict dest, const void *__restrict src, __SIZE_TYPE__ n);
#endif

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
** f3: { target { { any-opts "-mcmodel=medlow" } && { no-opts "-march=rv64gcv_zvl512b" "-march=rv64gcv_zvl1024b" "-mrvv-max-lmul=dynamic" "-mrvv-max-lmul=m2" "-mrvv-max-lmul=m4" "-mrvv-max-lmul=m8" "-mrvv-vector-bits=zvl" } } }
**        lui\s+[ta][0-7],%hi\(a_a\)
**        addi\s+[ta][0-7],[ta][0-7],%lo\(a_a\)
**        lui\s+[ta][0-7],%hi\(a_b\)
**        addi\s+a4,[ta][0-7],%lo\(a_b\)
**        vsetivli\s+zero,16,e32,m8,ta,ma
**        vle32.v\s+v\d+,0\([ta][0-7]\)
**        vse32\.v\s+v\d+,0\([ta][0-7]\)
**        ret
*/

/*
** f3: { target { { any-opts "-mcmodel=medlow -mrvv-vector-bits=zvl" "-mcmodel=medlow -march=rv64gcv_zvl512b -mrvv-vector-bits=zvl" } && { no-opts "-march=rv64gcv_zvl1024b" } } }
**        lui\s+[ta][0-7],%hi\(a_a\)
**        lui\s+[ta][0-7],%hi\(a_b\)
**        addi\s+[ta][0-7],[ta][0-7],%lo\(a_a\)
**        addi\s+a4,[ta][0-7],%lo\(a_b\)
**        vl(1|4|2)re32\.v\s+v\d+,0\([ta][0-7]\)
**        vs(1|4|2)r\.v\s+v\d+,0\([ta][0-7]\)
**        ret
*/

/*
** f3: { target { { any-opts "-mcmodel=medlow -march=rv64gcv_zvl1024b" "-mcmodel=medlow -march=rv64gcv_zvl512b" } && { no-opts "-mrvv-vector-bits=zvl" } } }
**        lui\s+[ta][0-7],%hi\(a_a\)
**        lui\s+[ta][0-7],%hi\(a_b\)
**        addi\s+a4,[ta][0-7],%lo\(a_b\)
**        vsetivli\s+zero,16,e32,(m1|m4|mf2),ta,ma
**        vle32.v\s+v\d+,0\([ta][0-7]\)
**        addi\s+[ta][0-7],[ta][0-7],%lo\(a_a\)
**        vse32\.v\s+v\d+,0\([ta][0-7]\)
**        ret
*/

/*
** f3: { target { { any-opts "-mcmodel=medany" } && { no-opts "-march=rv64gcv_zvl512b" "-march=rv64gcv_zvl256b" "-march=rv64gcv_zvl1024b" "-mrvv-max-lmul=dynamic" "-mrvv-max-lmul=m8" "-mrvv-max-lmul=m4" "-mrvv-vector-bits=zvl" } } }
**        lla\s+[ta][0-7],a_a
**        lla\s+[ta][0-7],a_b
**        vsetivli\s+zero,16,e32,m8,ta,ma
**        vle32.v\s+v\d+,0\([ta][0-7]\)
**        vse32\.v\s+v\d+,0\([ta][0-7]\)
**        ret
*/

/*
** f3: { target { { any-opts "-mcmodel=medany"  } && { no-opts "-march=rv64gcv_zvl512b" "-march=rv64gcv_zvl256b" "-march=rv64gcv" "-march=rv64gc_zve64d" "-march=rv64gc_zve32f" } } }
**        lla\s+[ta][0-7],a_b
**        vsetivli\s+zero,16,e32,m(f2|1|4),ta,ma
**        vle32.v\s+v\d+,0\([ta][0-7]\)
**        lla\s+[ta][0-7],a_a
**        vse32\.v\s+v\d+,0\([ta][0-7]\)
**        ret
*/

/*
** f3: { target { { any-opts "-mcmodel=medany -mrvv-vector-bits=zvl" } && { no-opts "-march=rv64gcv_zvl1024b" } } }
**        lla\s+[ta][0-7],a_a
**        lla\s+[ta][0-7],a_b
**        vl(1|2|4)re32\.v\s+v\d+,0\([ta][0-7]\)
**        vs(1|2|4)r\.v\s+v\d+,0\([ta][0-7]\)
**        ret
*/

extern struct { __INT32_TYPE__ a[16]; } a_a, a_b;

void f3 ()
{
  memcpy (&a_a, &a_b, sizeof a_a);
}

/* { dg-final { scan-assembler-not {\m(tail|call)\s+memcpy\M} } } */
