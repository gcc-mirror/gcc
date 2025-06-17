/* { dg-do compile } */
/* { dg-options "-march=rv32i_zilsd -mabi=ilp32" } */

long long y;
long long foo(long long x)
{
    return y >> x;
}
/* TODO: We should not split that 64 bit load into two 32 bit load if we have
   zilsd, but we split that during the expand time, so it's hard to fix via cost
   model turning, we could either fix that for expander, or...combine those two
   32 bit load back later.  */
/* { dg-final { scan-assembler-times "ld\t" 1 { xfail riscv*-*-*  } } } */

/* Os and Oz will use libcall, so the 64 bit load won't be split.  */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Oz" } } */
