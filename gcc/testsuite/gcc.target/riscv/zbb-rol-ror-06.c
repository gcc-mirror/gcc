/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64d -O2" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-final { scan-assembler-not {\mand} } } */

/*
**foo1:
**	roriw	a0,a0,14
**	ret
*/
unsigned int foo1 (unsigned int rs1)
{ return ((rs1 >> 14) | (rs1 << 18)); }

/*
**foo2:
**	roriw	a0,a0,18
**	ret
*/
unsigned int foo2 (unsigned int rs1)
{ return ((rs1 >> 18) | (rs1 << 14)); }

/*
**foo3:
**	roriw	a0,a0,18
**	ret
*/
unsigned int foo3 (unsigned int rs1)
{ return ((rs1 << 14) | (rs1 >> 18)); }

/*
**foo4:
**	roriw	a0,a0,14
**	ret
*/
unsigned int foo4 (unsigned int rs1)
{ return ((rs1 << 18) | (rs1 >> 14)); }
