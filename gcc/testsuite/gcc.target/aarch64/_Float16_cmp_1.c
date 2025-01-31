/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.2-a+fp16" } */

/*
** test_fcmp_store:
**	fcmp	h0, h1
**	cset	w0, eq
**	ret
*/
int
test_fcmp_store(_Float16 a, _Float16 b)
{
    return a == b;
}

/*
** test_fcmpe_store:
**	fcmpe	h0, h1
**	cset	w0, mi
**	ret
*/
int
test_fcmpe_store(_Float16 a, _Float16 b)
{
    return a < b;
}

/*
** test_fcmp_branch:
**	fcmp	h0, h1
**	...
*/
_Float16
test_fcmp_branch(_Float16 a, _Float16 b)
{
    if (a == b)
        return a * b;
    return a;
}

/*
** test_fcmpe_branch:
**	fcmpe	h0, h1
**	...
*/
_Float16
test_fcmpe_branch(_Float16 a, _Float16 b)
{
    if (a < b)
        return a * b;
    return a;
}

/* { dg-final { check-function-bodies "**" "" "" } } */