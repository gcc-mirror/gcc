/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_xtheadcondmov -mabi=lp64d -mriscv-attribute" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-Os" "-Og" "-O3" "-Oz" "-flto"} } */
/* { dg-final { check-function-bodies "**" ""  } } */

/*
**ConEmv_imm_imm_reg:
**	addi\t\s*[a-x0-9]+,\s*[a-x0-9]+,-1000+
**	li\t\s*[a-x0-9]+,10+
**	th.mvnez\t\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**	ret
*/
int ConEmv_imm_imm_reg(int x, int y){
  if (x == 1000) return 10;
  return y;
}

/*
**ConEmv_imm_reg_reg:
**	addi\t\s*[a-x0-9]+,\s*[a-x0-9]+,-1000+
**	th.mveqz\t\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**	mv\t\s*[a-x0-9]+,\s*[a-x0-9]+
**	ret
*/
int ConEmv_imm_reg_reg(int x, int y, int z){
  if (x == 1000) return y;
  return z;
}

/*
**ConEmv_reg_imm_reg:
**	sub\t\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**	li\t\s*[a-x0-9]+,10+
**	th.mvnez\t\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**	ret
*/
int ConEmv_reg_imm_reg(int x, int y, int z){
  if (x == y) return 10;
  return z;
}

/*
**ConEmv_reg_reg_reg:
**	sub\t\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**	th.mveqz\t\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**	mv\t\s*[a-x0-9]+,\s*[a-x0-9]+
**	ret
*/
int ConEmv_reg_reg_reg(int x, int y, int z, int n){
  if (x == y) return z;
  return n;
}

/*
**ConNmv_imm_imm_reg:
**	addi\t\s*[a-x0-9]+,\s*[a-x0-9]+,-1000+
**	li\t\s*[a-x0-9]+,9998336+
**	addi\t\s*[a-x0-9]+,\s*[a-x0-9]+,1664+
**	th.mveqz\t\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**	ret
*/
int ConNmv_imm_imm_reg(int x, int y){
  if (x != 1000) return 10000000;
  return y;
}

/*
**ConNmv_imm_reg_reg:
**	addi\t\s*[a-x0-9]+,\s*[a-x0-9]+,-1000+
**	th.mvnez\t\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**	mv\t\s*[a-x0-9]+,\s*[a-x0-9]+
**	ret
*/
int ConNmv_imm_reg_reg(int x, int y, int z){
  if (x != 1000) return y;
  return z;
}

/*
**ConNmv_reg_imm_reg:
**	sub\t\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**	li\t\s*[a-x0-9]+,10+
**	th.mveqz\t\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**	ret
*/
int ConNmv_reg_imm_reg(int x, int y, int z){
  if (x != y) return 10;
  return z;
}

/*
**ConNmv_reg_reg_reg:
**	sub\t\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**	th.mvnez\t\s*[a-x0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+
**	mv\t\s*[a-x0-9]+,\s*[a-x0-9]+
**	ret
*/
int ConNmv_reg_reg_reg(int x, int y, int z, int n){
  if (x != y) return z;
  return n;
}


/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0_xtheadcondmov1p0\"" } } */
