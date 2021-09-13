/* { dg-do assemble { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "-O3 --save-temps -std=gnu90 -mfloat-abi=hard" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_neon.h>

/*
**stacktest1:
**	...
**	vst1.16	{d[0-9]+\[[0-9]+\]}, \[r[0-9]+\]
**	vld1.16	{d[0-9]+\[[0-9]+\]}, \[r[0-9]+\]
**	...
**	bx	lr
*/
bfloat16_t stacktest1 (bfloat16_t __a)
{
  volatile bfloat16_t b = __a;
  return b;
}

/*
**stacktest2:
**	...
**	vstr	d[0-9]+, \[sp\]
**	vldr	d[0-9]+, \[sp\]
**	...
**	bx	lr
*/
bfloat16x4_t stacktest2 (bfloat16x4_t __a)
{
  volatile bfloat16x4_t b = __a;
  return b;
}

/*
**stacktest3:
**	...
**	vst1.64	{d[0-9]+-d[0-9]+}, \[sp:[0-9]+\]
**	vld1.64	{d[0-9]+-d[0-9]+}, \[sp:[0-9]+\]
**	...
**	bx	lr
*/
bfloat16x8_t stacktest3 (bfloat16x8_t __a)
{
  volatile bfloat16x8_t b = __a;
  return b;
}

/*  Test compilation of __attribute__ vectors of 8, 16, 32, etc. BFloats.  */
typedef bfloat16_t v8bf __attribute__((vector_size(16)));
typedef bfloat16_t v16bf __attribute__((vector_size(32)));
typedef bfloat16_t v32bf __attribute__((vector_size(64)));
typedef bfloat16_t v64bf __attribute__((vector_size(128)));
typedef bfloat16_t v128bf __attribute__((vector_size(256)));

v8bf stacktest4 (v8bf __a)
{
  volatile v8bf b = __a;
  return b;
}

v16bf stacktest5 (v16bf __a)
{
  volatile v16bf b = __a;
  return b;
}

v32bf stacktest6 (v32bf __a)
{
  volatile v32bf b = __a;
  return b;
}

v64bf stacktest7 (v64bf __a)
{
  volatile v64bf b = __a;
  return b;
}

v128bf stacktest8 (v128bf __a)
{
  volatile v128bf b = __a;
  return b;
}

/* Test use of constant values to assign values to vectors.  */

typedef bfloat16_t v2bf __attribute__((vector_size(4)));
v2bf c2 (void) { return (v2bf) 0x12345678; }
