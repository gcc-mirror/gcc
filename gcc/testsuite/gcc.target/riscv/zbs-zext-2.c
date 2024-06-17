/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-O1" } } */
unsigned long long foo(long long symbol)
{
        return 1u << symbol;
}

/* { dg-final { scan-assembler-times "bset\t" 1 } } */
/* { dg-final { scan-assembler-not "li\t"} } */
/* { dg-final { scan-assembler-not "sllw\t"} } */
/* { dg-final { scan-assembler-not "zext.w\t"} } */
