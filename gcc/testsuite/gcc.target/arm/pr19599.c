/* { dg-skip-if "need at least armv5te" { *-*-* } { "-march=armv[234]*" "-mthumb" } { "" } } */
/* { dg-options "-O2 -march=armv5te -marm" }  */
/* { dg-final { scan-assembler "bx" } } */

int (*indirect_func)();

int indirect_call()
{
  return indirect_func();
}
