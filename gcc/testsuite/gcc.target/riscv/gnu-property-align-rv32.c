/* { dg-do compile } */
/* { dg-options "-march=rv32g_zicfiss -fcf-protection=return -mabi=ilp32d " } */

void foo() {}

/* { dg-final { scan-assembler-times ".p2align\t2" 3 } } */
/* { dg-final { scan-assembler-not ".p2align\t3" } } */
