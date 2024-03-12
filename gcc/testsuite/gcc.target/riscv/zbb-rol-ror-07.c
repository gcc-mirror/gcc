/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64d -fno-lto -O2" } */
/* { dg-skip-if "" { *-*-* } { "-g" } } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-final { scan-assembler-not {\mand} } } */

/*
**foo1:
**	rori	a0,a0,34
**	ret
*/
unsigned long foo1 (unsigned long rs1)
{
    unsigned long tempt;
    tempt = rs1 >> 30;
    tempt = tempt << 2;
    tempt = tempt >> 6;
    rs1 = tempt | (rs1 << 30);
    return rs1 ; 
}

/*
**foo2:
**	rori	a0,a0,24
**	ret
*/
unsigned long foo2 (unsigned long rs1)
{
    unsigned long tempt;
    tempt = rs1 >> 20;
    tempt = tempt << 2;
    tempt = tempt >> 6;
    rs1 = tempt | (rs1 << 40);
    return rs1 ; 
}

/*
**foo3:
**	rori	a0,a0,40
**	ret
*/
unsigned long foo3 (unsigned long rs1)
{
    unsigned long tempt;
    tempt = rs1 << 20;
    tempt = tempt >> 2;
    tempt = tempt << 6;
    rs1 = tempt | (rs1 >> 40);
    return rs1 ; 
}

/*
**foo4:
**	rori	a0,a0,20
**	ret
*/
unsigned long foo4 (unsigned long rs1)
{
    unsigned long tempt;
    tempt = rs1 << 40;
    tempt = tempt >> 2;
    tempt = tempt << 6;
    rs1 = tempt | (rs1 >> 20);
    return rs1 ; 
}
