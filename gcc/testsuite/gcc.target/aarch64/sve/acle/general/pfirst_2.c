/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_sve.h>

#ifdef __cplusplus
extern "C" {
#endif

extern void foo (svbool_t);
extern void bar (svbool_t);

/*
** test1:
**	b	bar
*/
__attribute__ ((noipa))
void test1 (svbool_t a)
{
    svbool_t res = svpfirst (svpfalse (), svpfalse ());
    if (svptest_any (res, a))
      foo (a);
    else
      bar (a);
}

/*
** test2:
**	pfalse	p0\.b
**	ret
*/
__attribute__ ((noipa))
svbool_t test2 (svbool_t a)
{
    return svpfirst (svpfalse (), svpfalse ());
}

/*
** test3:
**	ptrues	p3.b, vl1
**	...
*/
__attribute__ ((noipa))
void test3 (svbool_t a)
{
    svbool_t res = svpfirst (svptrue_b8 (), svpfalse ());
    if (svptest_first (svptrue_b8 (), res))
      foo (a);
    else
      bar (a);
}

/*
** test4:
**	ptrue	p0.b, vl1
**	ret
*/
__attribute__ ((noipa))
svbool_t test4 (svbool_t a)
{
return svpfirst (svptrue_b8 (), svpfalse ());
}

#ifdef __cplusplus
}
#endif
