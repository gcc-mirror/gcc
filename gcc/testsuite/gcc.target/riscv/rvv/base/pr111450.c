/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvfh -mabi=ilp32d -O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

typedef _Float16 float16_t;
typedef float float32_t;
typedef double float64_t;

/*
**foo:
**	vsetvli\s+zero,\s*[a-z0-9]+,e8,m1,ta,ma
**	vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	vse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	ret
*/
void foo (int8_t *in, int8_t *out, int n)
{
    vint8m1_t v = __riscv_vlse8_v_i8m1 (in, 1, n);
    __riscv_vsse8_v_i8m1 (out, 1, v, n);
}

/*
**foo1:
**	vsetvli\s+zero,\s*[a-z0-9]+,e16,m1,ta,ma
**	vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	vse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	ret
*/
void foo1 (int16_t *in, int16_t *out, int n)
{
    vint16m1_t v = __riscv_vlse16_v_i16m1 (in, 2, n);
    __riscv_vsse16_v_i16m1 (out, 2, v, n);
}

/*
**foo2:
**	vsetvli\s+zero,\s*[a-z0-9]+,e32,m1,ta,ma
**	vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	vse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	ret
*/
void foo2 (int32_t *in, int32_t *out, int n)
{
    vint32m1_t v = __riscv_vlse32_v_i32m1 (in, 4, n);
    __riscv_vsse32_v_i32m1 (out, 4, v, n);
}

/*
**foo3:
**	vsetvli\s+zero,\s*[a-z0-9]+,e64,m1,ta,ma
**	vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	vse64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	ret
*/
void foo3 (int64_t *in, int64_t *out, int n)
{
    vint64m1_t v = __riscv_vlse64_v_i64m1 (in, 8, n);
    __riscv_vsse64_v_i64m1 (out, 8, v, n);
}

/*
**foo4:
**	vsetvli\s+zero,\s*[a-z0-9]+,e16,mf2,ta,ma
**	vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	vse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	ret
*/
void foo4 (float16_t *in, float16_t *out, int n)
{
    vfloat16mf2_t v = __riscv_vlse16_v_f16mf2 (in, 2, n);
    __riscv_vsse16_v_f16mf2 (out, 2, v, n);
}

/*
**foo5:
**	vsetvli\s+zero,\s*[a-z0-9]+,e32,m1,ta,ma
**	vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	vse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	ret
*/
void foo5 (float32_t *in, float32_t *out, int n)
{
    vfloat32m1_t v = __riscv_vlse32_v_f32m1 (in, 4, n);
    __riscv_vsse32_v_f32m1 (out, 4, v, n);
}

/*
**foo6:
**	vsetvli\s+zero,\s*[a-z0-9]+,e64,m1,ta,ma
**	vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	vse64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
**	ret
*/
void foo6 (float64_t *in, float64_t *out, int n)
{
    vfloat64m1_t v = __riscv_vlse64_v_f64m1 (in, 8, n);
    __riscv_vsse64_v_f64m1 (out, 8, v, n);
}
