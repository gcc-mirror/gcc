/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */


_Bool match(const int ch, int fMap) {
    return ((fMap & (1<<(ch))) == 0);
}

_Bool match2(const int ch, int fMap) {
    return ((fMap & (1UL<<(ch))) == 0);
}


/* { dg-final { scan-assembler-times "bext\t" 2 } } */
/* { dg-final { scan-assembler-times "seqz\t|xori\t" 2 } } */
/* { dg-final { scan-assembler-not "sraw\t" } } */
/* { dg-final { scan-assembler-not "not\t" } } */
/* { dg-final { scan-assembler-not "andi\t" } } */
