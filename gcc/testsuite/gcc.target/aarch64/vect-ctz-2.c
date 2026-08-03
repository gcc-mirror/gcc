/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "vect-ctz.h"

/* With SVE the 0x80 folds into an immediate ORR, but the fixed-length loop
   still counts with the Advanced SIMD RBIT and CLZ.  */
/*
** ctzb:
**	...
**	ldr	q([0-9]+), \[x[0-9]+\]
**	orr	z([0-9]+)\.b, z\1\.b, -128
**	rbit	v([0-9]+)\.16b, v\2\.16b
**	clz	v([0-9]+)\.16b, v\3\.16b
**	str	q\4, \[x[0-9]+\]
**	ret
*/

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

/* The variable-length loops are vectorised with SVE and count there instead.  */
/* { dg-final { scan-assembler-times {\trbit\tv[0-9]+\.16b, v[0-9]+\.16b} 2 } } */
/* { dg-final { scan-assembler-times {\tclz\tv[0-9]+\.16b, v[0-9]+\.16b} 1 } } */
/* { dg-final { scan-assembler-times {\trev16\tv[0-9]+\.16b, v[0-9]+\.16b} 1 } } */
/* { dg-final { scan-assembler-times {\tclz\tv[0-9]+\.8h, v[0-9]+\.8h} 1 } } */
/* { dg-final { scan-assembler-times {\trbit\tz[0-9]+\.b, p[0-9]+/m, z[0-9]+\.b} 1 } } */
/* { dg-final { scan-assembler-times {\tclz\tz[0-9]+\.b, p[0-9]+/m, z[0-9]+\.b} 1 } } */
/* { dg-final { scan-assembler-times {\trbit\tz[0-9]+\.h, p[0-9]+/m, z[0-9]+\.h} 1 } } */
/* { dg-final { scan-assembler-times {\tclz\tz[0-9]+\.h, p[0-9]+/m, z[0-9]+\.h} 1 } } */
