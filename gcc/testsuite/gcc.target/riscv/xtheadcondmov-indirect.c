/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadcondmov -fno-sched-pressure" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadcondmov -fno-sched-pressure" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** ConEmv_imm_imm_reg:
**	addi	a[0-9]+,a[0-9]+,-1000
**	li	a[0-9]+,10
**	th\.mvnez	a[0-9]+,a[0-9]+,a[0-9]+
**	ret
*/
int ConEmv_imm_imm_reg(int x, int y)
{
  if (x == 1000)
    return 10;
  return y;
}

/*
** ConEmv_imm_reg_reg:
**	addi	a[0-9]+,a[0-9]+,-1000
**	th.mveqz	a[0-9]+,a[0-9]+,a[0-9]+
**	mv	a[0-9]+,a[0-9]+
**	ret
*/
int ConEmv_imm_reg_reg(int x, int y, int z)
{
  if (x == 1000)
    return y;
  return z;
}

/*
** ConEmv_reg_imm_reg:
**	sub	a[0-9]+,a[0-9]+,a[0-9]+
**	li	a[0-9]+,10
**	th.mvnez	a[0-9]+,a[0-9]+,a[0-9]+
**	ret
*/
int ConEmv_reg_imm_reg(int x, int y, int z)
{
  if (x == y)
    return 10;
  return z;
}

/*
** ConEmv_reg_reg_reg:
**	sub	a[0-9]+,a[0-9]+,a[0-9]+
**	th.mveqz	a[0-9]+,a[0-9]+,a[0-9]+
**	mv	a[0-9]+,a[0-9]+
**	ret
*/
int ConEmv_reg_reg_reg(int x, int y, int z, int n)
{
  if (x == y)
    return z;
  return n;
}

/*
** ConNmv_imm_imm_reg:
**	addi	a[0-9]+,a[0-9]+,-1000+
**	li	a[0-9]+,9998336+
**	addi	a[0-9]+,a[0-9]+,1664+
**	th.mveqz	a[0-9]+,a[0-9]+,a[0-9]+
**	ret
*/
int ConNmv_imm_imm_reg(int x, int y)
{
  if (x != 1000)
    return 10000000;
  return y;
}

/*
**ConNmv_imm_reg_reg:
**	addi	a[0-9]+,a[0-9]+,-1000+
**	th.mvnez	a[0-9]+,a[0-9]+,a[0-9]+
**	mv	a[0-9]+,a[0-9]+
**	ret
*/
int ConNmv_imm_reg_reg(int x, int y, int z)
{
  if (x != 1000)
    return y;
  return z;
}

/*
**ConNmv_reg_imm_reg:
**	sub	a[0-9]+,a[0-9]+,a[0-9]+
**	li	a[0-9]+,10+
**	th.mveqz	a[0-9]+,a[0-9]+,a[0-9]+
**	ret
*/
int ConNmv_reg_imm_reg(int x, int y, int z)
{
  if (x != y)
    return 10;
  return z;
}

/*
**ConNmv_reg_reg_reg:
**	sub	a[0-9]+,a[0-9]+,a[0-9]+
**	th.mvnez	a[0-9]+,a[0-9]+,a[0-9]+
**	mv	a[0-9]+,a[0-9]+
**	ret
*/
int ConNmv_reg_reg_reg(int x, int y, int z, int n)
{
  if (x != y)
    return z;
  return n;
}
