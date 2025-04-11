/* { dg-do compile } */
/* { dg-options "-march=rv64g_zicfiss -fcf-protection=return -mabi=lp64d " } */

void foo() {}

/* { dg-final { scan-assembler-times ".p2align\t3" 3 } } */
/* { dg-final { scan-assembler-not ".p2align\t2" } } */
