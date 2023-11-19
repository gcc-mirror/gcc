/* { dg-do compile } */
/* { dg-options "-O2 -mlsx" } */

#define test(func, suffix) \
__typeof__ (1.##suffix) \
_##func##suffix (__typeof__ (1.##suffix) x) \
{ \
  return __builtin_##func##suffix (x); \
}

test (ceil, f)
test (ceil, )
test (floor, f)
test (floor, )
test (trunc, f)
test (trunc, )
test (roundeven, f)
test (roundeven, )
test (nearbyint, f)
test (nearbyint, )
test (rint, f)
test (rint, )

/* { dg-final { scan-assembler "\tvfrintrp\.s" } } */
/* { dg-final { scan-assembler "\tvfrintrm\.s" } } */
/* { dg-final { scan-assembler "\tvfrintrz\.s" } } */
/* { dg-final { scan-assembler "\tvfrintrne\.s" } } */
/* { dg-final { scan-assembler "\tvfrintrp\.d" } } */
/* { dg-final { scan-assembler "\tvfrintrm\.d" } } */
/* { dg-final { scan-assembler "\tvfrintrz\.d" } } */
/* { dg-final { scan-assembler "\tvfrintrne\.d" } } */

/* must do vreplvei first */
/* { dg-final { scan-assembler-times "\tvreplvei\.w\t\\\$vr0,\\\$vr0,0" 4 } } */
/* { dg-final { scan-assembler-times "\tvreplvei\.d\t\\\$vr0,\\\$vr0,0" 4 } } */

/* nearbyint is not allowed to rasie FE_INEXACT for decades */
/* { dg-final { scan-assembler "\tb\t%plt\\(nearbyint\\)" } } */
/* { dg-final { scan-assembler "\tb\t%plt\\(nearbyintf\\)" } } */

/* rint should just use basic FP operation */
/* { dg-final { scan-assembler "\tfrint\.s" } } */
/* { dg-final { scan-assembler "\tfrint\.d" } } */
