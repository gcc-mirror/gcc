/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-additional-options "-O1 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */
/* { dg-add-options riscv_v } */
/* { dg-final { check-function-bodies "**" "" } } */

typedef struct { char c[16]; } c16;
typedef struct { char c[32]; } c32;
typedef struct { short s; char c[30]; } s16;

/* A short struct copy can use vsetivli.
** f1: { target { no-opts "-mrvv-vector-bits=zvl" } }
**	vsetivli\s+zero,16,e8,m(1|f8|f2|f4),ta,ma
**	vle8.v\s+v1,0\(a1\)
**	vse8.v\s+v1,0\(a0\)
**	ret
*/

/*
** f1: { target {  { any-opts "-mrvv-vector-bits=zvl" } && { no-opts "-march=rv64gcv_zvl1024b" "-march=rv64gcv_zvl256b" "-march=rv64gcv_zvl512b -mrvv-max-lmul=dynamic" } } }
**	vl1re8.v\s+v1,0\(a1\)
**	vs1r.v\s+v1,0\(a0\)
**	ret
*/

void f1 (c16 *a, c16* b)
{
  *a = *b;
}

/* A longer one needs li.
** f2: { target { no-opts "-mrvv-vector-bits=zvl" } }
**	li\s+[ta][0-7],32
**	vsetvli\s+zero,[ta][0-7],e8,m(f4|f2|1|2|8),ta,ma
**	vle8.v\s+v(1|2|8),0\(a1\)
**	vse8.v\s+v(1|2|8),0\(a0\)
**	ret
*/

/*
** f2: { target {  { any-opts "-mrvv-vector-bits=zvl" } && { no-opts "-march=rv64gcv_zvl1024b" "-march=rv64gcv_zvl256b" "-march=rv64gcv_zvl512b -mrvv-max-lmul=dynamic" } } }
**	vl2re8.v\s+v2,0\(a1\)
**	vs2r.v\s+v2,0\(a0\)
**	ret
*/
void f2 (c32 *a, c32* b)
{
  *a = *b;
}

/* A 32 byte struct is still short enough for vsetivli
   if we can use an element width larger than 8.
** f3: { target { no-opts "-mrvv-vector-bits=zvl" } }
**	vsetivli\s+zero,16,e16,m(f2|f4|1|2|8),ta,ma
**	vle16.v\s+v(1|2|8),0\(a1\)
**	vse16.v\s+v(1|2|8),0\(a0\)
**	ret
*/

/*
** f3: { target {  { any-opts "-mrvv-vector-bits=zvl" } && { no-opts "-march=rv64gcv_zvl1024b" "-march=rv64gcv_zvl256b" "-march=rv64gcv_zvl512b -mrvv-max-lmul=dynamic" } } }
**	vl2re16.v\s+v2,0\(a1\)
**	vs2r.v\s+v2,0\(a0\)
**	ret
*/
void f3 (s16 *a, s16* b)
{
  *a = *b;
}
