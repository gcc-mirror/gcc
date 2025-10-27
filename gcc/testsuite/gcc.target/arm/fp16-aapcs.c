/* { dg-do compile }  */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-O2" }  */
/* { dg-add-options arm_fp16_alternative } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/* Test __fp16 arguments and return value in registers.  */

__fp16 f();
void g(__fp16);
void h();

/*
** test:
** ...
**	bl	f
**	bl	g
** ...
*/
int test()
{
    g(f());
    h();
}
