/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba_zbb -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

int f(int x) {
 return x >= 0 ? x : 0;
}

/* { dg-final { scan-assembler-times "max\t" 1 } } */
/* { dg-final { scan-assembler-not "li\t" } } */
