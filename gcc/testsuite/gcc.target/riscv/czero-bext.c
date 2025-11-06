/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gbc_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-O2 -march=rv32gbc_zicond -mabi=ilp32" { target { rv32 } } } */

bool isValidAncestorType(int type) {
    if (type == 0 || type == 6 || type == 4) {
            return true;
    }
    return false;
}



/* { dg-final { scan-assembler "czero.nez\t" } } */
/* { dg-final { scan-assembler "sgtu\t" } } */
/* { dg-final { scan-assembler-not "bgtu\t" } } */

