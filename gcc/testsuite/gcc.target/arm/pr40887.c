/* { dg-skip-if "need at least armv5" { *-*-* } { "-march=armv[234]*" } { "" } } */
/* { dg-options "-O2 -march=armv5te" }  */
/* { dg-final { scan-assembler "blx" } } */

int (*indirect_func)();

int indirect_call()
{
    return indirect_func();
}
