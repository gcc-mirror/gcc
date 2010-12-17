/* { dg-do compile } */
/* { dg-skip-if "128-bit long double" { *-*-hpux* } { "*" } { "" } } */
/* { dg-options "-O" } */
/* Don't confuse the fma insn with the fma in the filename.  */
/* { dg-final { scan-assembler-times "fma\[ 	\]" 2 } } */
/* { dg-final { scan-assembler-times "fms" 1 } } */
/* { dg-final { scan-assembler-times "fnma" 2 } } */

#ifndef __FP_FAST_FMAL
# error "__FP_FAST_FMAL should be defined"
#endif

typedef long double LD;

LD L0(LD x, LD y, LD z) { return __builtin_fmal(x,y,z); }
LD L1(LD x, LD y, LD z) { return __builtin_fmal(x,y,-z); }
LD L2(LD x, LD y, LD z) { return __builtin_fmal(-x,y,z); }
LD L3(LD x, LD y, LD z) { return __builtin_fmal(x,-y,z); }
LD L4(LD x, LD y, LD z) { return __builtin_fmal(-x,-y,z); }
