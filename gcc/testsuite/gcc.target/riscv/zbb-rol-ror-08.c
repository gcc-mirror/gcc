/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64d -O2" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-final { scan-assembler-not {\mand} } } */

/*
**foo1:
**	rori	a0,a0,32
**	ret
*/
unsigned long foo1(unsigned long rotate)
{
    return (rotate << 32) | (rotate >> 32);
}

/*
**foo2:
**	roriw	a0,a0,16
**	ret
*/
unsigned int foo2(unsigned int rotate)
{
    return (rotate << 16) | (rotate >> 16);
}
