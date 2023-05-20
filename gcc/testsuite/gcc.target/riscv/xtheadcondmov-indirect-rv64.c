/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_xtheadcondmov -mabi=lp64d -mriscv-attribute" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" } } */
/* { dg-final { check-function-bodies "**" ""  } } */

/*
**ConEmv_imm_imm_reg:
**	addi	a5,a0,-1000
**	li	a0,10
**	th.mvnez	a0,zero,a5
**	th.mveqz	a1,zero,a5
**	or	a0,a0,a1
**	ret
*/
int ConEmv_imm_imm_reg(int x, int y){
  if (x == 1000) return 10;
  return y;
}

/*
**ConEmv_imm_reg_reg:
**	addi	a5,a0,-1000
**	th.mvnez	a1,zero,a5
**	th.mveqz	a2,zero,a5
**	or	a0,a1,a2
**	ret
*/
int ConEmv_imm_reg_reg(int x, int y, int z){
  if (x == 1000) return y;
  return z;
}

/*
**ConEmv_reg_imm_reg:
**	sub	a1,a0,a1
**	li	a0,10
**	th.mvnez	a0,zero,a1
**	th.mveqz	a2,zero,a1
**	or	a0,a0,a2
**	ret
*/
int ConEmv_reg_imm_reg(int x, int y, int z){
  if (x == y) return 10;
  return z;
}

/*
**ConEmv_reg_reg_reg:
**	sub	a1,a0,a1
**	th.mvnez	a2,zero,a1
**	th.mveqz	a3,zero,a1
**	or	a0,a2,a3
**	ret
*/
int ConEmv_reg_reg_reg(int x, int y, int z, int n){
  if (x == y) return z;
  return n;
}

/*
**ConNmv_imm_imm_reg:
**	li	a5,9998336
**	addi	a4,a0,-1000
**	addi	a5,a5,1664
**	th.mvnez	a1,zero,a4
**	th.mveqz	a5,zero,a4
**	or	a0,a1,a5
**	ret
*/
int ConNmv_imm_imm_reg(int x, int y){
  if (x != 1000) return 10000000;
  return y;
}

/*
**ConNmv_imm_reg_reg:
**	addi	a5,a0,-1000
**	th.mveqz	a1,zero,a5
**	th.mvnez	a2,zero,a5
**	or	a0,a1,a2
**	ret
*/
int ConNmv_imm_reg_reg(int x, int y, int z){
  if (x != 1000) return y;
  return z;
}

/*
**ConNmv_reg_imm_reg:
**	sub	a1,a0,a1
**	li	a0,10
**	th.mveqz	a0,zero,a1
**	th.mvnez	a2,zero,a1
**	or	a0,a0,a2
**	ret
*/
int ConNmv_reg_imm_reg(int x, int y, int z){
  if (x != y) return 10;
  return z;
}

/*
**ConNmv_reg_reg_reg:
**	sub	a1,a0,a1
**	th.mveqz	a2,zero,a1
**	th.mvnez	a3,zero,a1
**	or	a0,a2,a3
**	ret
*/
int ConNmv_reg_reg_reg(int x, int y, int z, int n){
  if (x != y) return z;
  return n;
}


/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0_xtheadcondmov1p0\"" } } */
