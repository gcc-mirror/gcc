/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "vect-ctz.h"

/* Without SVE the 0x80 needs a vector constant of its own.  */
/*
** ctzb:
**	...
**	movi	v([0-9]+)\.16b, 0xffffffffffffff80
**	ldr	q([0-9]+), \[x[0-9]+\]
**	orr	v([0-9]+)\.16b, v\2\.16b, v\1\.16b
**	rbit	v([0-9]+)\.16b, v\3\.16b
**	clz	v([0-9]+)\.16b, v\4\.16b
**	str	q\5, \[x[0-9]+\]
**	ret
*/

/* A halfword element needs REV16 as well as RBIT.  The 0x8000 fits the
   immediate form of ORR here.  */
/*
** ctzh:
**	...
**	ldr	q([0-9]+), \[x[0-9]+\]
**	orr	v\1\.8h, #128, lsl #8
**	rev16	v([0-9]+)\.16b, v\1\.16b
**	rbit	v([0-9]+)\.16b, v\2\.16b
**	clz	v([0-9]+)\.8h, v\3\.8h
**	str	q\4, \[x[0-9]+\]
**	ret
*/

/* Both the fixed-length and the variable-length loop count this way, for each
   of the two element sizes.  */
/* { dg-final { scan-assembler-times {\trbit\tv[0-9]+\.16b, v[0-9]+\.16b} 4 } } */
/* { dg-final { scan-assembler-times {\tclz\tv[0-9]+\.16b, v[0-9]+\.16b} 2 } } */
/* { dg-final { scan-assembler-times {\trev16\tv[0-9]+\.16b, v[0-9]+\.16b} 2 } } */
/* { dg-final { scan-assembler-times {\tclz\tv[0-9]+\.8h, v[0-9]+\.8h} 2 } } */
