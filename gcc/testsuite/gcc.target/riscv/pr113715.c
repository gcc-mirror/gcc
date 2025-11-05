/* { dg-do compile } */
/* { dg-options "-march=rv32ima_zca_zcmp -mabi=ilp32 -mcmodel=medlow -fno-pic" }*/
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-O2" "-Og" "-O3" "-flto"} } */
/* { dg-final { check-function-bodies "**" "" } } */

void test_1(int);

/*
**test_err:
** ...
**	li	a0,1
**	call	test_1
**	cm.popretz	{ra}, 16
** ...
*/
int test_err(int mode)
{
    if (mode == 2) {
        test_1(1);
    }

    return 0;
}

/*
**test_err2:
** ...
**	li	a0,1
**	call	test_1
**	li	a1,0
**	cm.popretz	{ra}, 16
** ...
*/
long long test_err2(int mode)
{
    if (mode == 2) {
        test_1(1);
    }

    return 0;
}


/*
**test_err3:
** ...
**	li	a0,1
**	call	test_1
**	li	a1,1
**	cm.popretz	{ra}, 16
** ...
*/
long long test_err3(int mode)
{
    if (mode == 2) {
        test_1(1);
	return 0x100000000ll;
    }

    return 0;
}

/*
**test_err4:
** ...
**	li	a0,1
**	call	test_1
**	cm.popretz	{ra}, 16
** ...
*/
float test_err4(int mode)
{
    if (mode == 2) {
        test_1(1);
	return 0.0f;
    }

    return 1.0f;
}

/*
**test_err5:
** ...
**	li	a0,1
**	call	test_1
**	li	a1,0
**	cm.popretz	{ra}, 16
** ...
*/
double test_err5(int mode)
{
    if (mode == 2) {
        test_1(1);
	return 0.0;
    }

    return 1.0;
}
