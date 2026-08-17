/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_sve.h>

/*
** f1m:
**	mov	p0.b, p1.b
**	ret
*/
__attribute__ ((noipa))
svbool_t f1m (svbool_t a, svbool_t b)
{
    return svbrka_b_m (b, svpfalse_b(), a);
}

/*
** f1z:
**	pfalse	p0.b
**	ret
*/
__attribute__ ((noipa))
svbool_t f1z (svbool_t a)
{
    return svbrka_b_z (svpfalse_b(), a);
}

/*
** f2m:
**	ptrue	p3.b, all
**	brka	p1.b, p3/m, p0.b
**	mov	p0.b, p1.b
**	ret
*/
__attribute__ ((noipa))
svbool_t f2m (svbool_t a, svbool_t b)
{
    return svbrka_b_m (b, svptrue_b8(), a);
}

/*
** f2z:
**	ptrue	p3.b, all
**	brka	p0.b, p3/z, p0.b
**	ret
*/
__attribute__ ((noipa))
svbool_t f2z (svbool_t a)
{
    return svbrka_b_z (svptrue_b8(), a);
}

/*
** f3z:
**	ptrue	p0.b, vl1
**	ret
*/
__attribute__ ((noipa))
svbool_t f3z (svbool_t a)
{
    return svbrka_b_z (svptrue_b8(), svptrue_b8());
}

/*
** f3m:
**	ptrue	p3.b, all
**	brka	p0.b, p3/m, p0.b
**	ret
*/
__attribute__ ((noipa))
svbool_t f3m (svbool_t a, svbool_t b)
{
    return svbrka_b_m (a, svptrue_b8(), a);
}

/*
** f4m:
**	ptrue	p0.b, vl1
**	ret
*/
__attribute__ ((noipa))
svbool_t f4m (svbool_t a)
{
    return svbrka_b_m (a, svptrue_b8(), svptrue_b8 ());
}

/*
** f4z:
**	ptrue	p0.b, all
**	ret
*/
__attribute__ ((noipa))
svbool_t f4z ()
{
    return svbrka_b_z (svptrue_b8(), svpfalse());
}

/*
** g1z:
**	pfalse	p0.b
**	ret
*/
__attribute__ ((noipa))
svbool_t g1z (svbool_t a)
{
    return svbrkb_b_z (svpfalse_b(), a);
}

/*
** g1m:
**	ptrue	p3.b, all
**	brkb	p0.b, p3/z, p0.b
**	ret
*/
__attribute__ ((noipa))
svbool_t g1m (svbool_t a)
{
    return svbrkb_b_m (svpfalse_b(), svptrue_b8(), a);
}

/*
** g2z:
**	ptrue	p3.b, all
**	brkb	p0.b, p3/z, p0.b
**	ret
*/
__attribute__ ((noipa))
svbool_t g2z (svbool_t a)
{
    return svbrkb_b_z (svptrue_b8(), a);
}

/*
** g2m:
**	ptrue	p0.b, all
**	ret
*/
__attribute__ ((noipa))
svbool_t g2m (svbool_t a)
{
    return svbrkb_b_m (a, svptrue_b8(), svpfalse ());
}

/*
** g3m:
**	pfalse	p0.b
**	ret
*/
__attribute__ ((noipa))
svbool_t g3m (svbool_t a)
{
    return svbrkb_b_m (a, svptrue_b8(), svptrue_b8 ());
}

int main ()
{
    svbool_t a = svptrue_pat_b16 (SV_VL4);
    svbool_t b = svptrue_pat_b16 (SV_VL5);

    f1m (a, b);
    f1z (a);
    f2m (a, b);
    f2z (a);
    f3m (a, b);
    f3z (a);
    f4m (a);
    f4z ();
    g1z (a);
    g1m (a);
    g2z (a);
    g2m (a);
    g3m (a);
}