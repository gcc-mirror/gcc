/* { dg-do compile } */
/* { dg-options "-O2 -mlsx -ffixed-f0 -ffixed-f1 -ffixed-f2 -fno-vect-cost-model" } */

#define F double
#define I long long

#include "vfcmp-f.c"

/* { dg-final { scan-assembler "compare_quiet_equal:.*\tvfcmp\\.ceq\\.d\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_equal\n" } } */
/* { dg-final { scan-assembler "compare_quiet_not_equal:.*\tvfcmp\\.cune\\.d\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_not_equal\n" } } */
/* { dg-final { scan-assembler "compare_signaling_greater:.*\tvfcmp\\.slt\\.d\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_signaling_greater\n" } } */
/* { dg-final { scan-assembler "compare_signaling_greater_equal:.*\tvfcmp\\.sle\\.d\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_signaling_greater_equal\n" } } */
/* { dg-final { scan-assembler "compare_signaling_less:.*\tvfcmp\\.slt\\.d\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_signaling_less\n" } } */
/* { dg-final { scan-assembler "compare_signaling_less_equal:.*\tvfcmp\\.sle\\.d\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_signaling_less_equal\n" } } */
/* { dg-final { scan-assembler "compare_signaling_not_greater:.*\tvfcmp\\.sule\\.d\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_signaling_not_greater\n" } } */
/* { dg-final { scan-assembler "compare_signaling_less_unordered:.*\tvfcmp\\.sult\\.d\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_signaling_less_unordered\n" } } */
/* { dg-final { scan-assembler "compare_signaling_not_less:.*\tvfcmp\\.sule\\.d\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_signaling_not_less\n" } } */
/* { dg-final { scan-assembler "compare_signaling_greater_unordered:.*\tvfcmp\\.sult\\.d\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_signaling_greater_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_less:.*\tvfcmp\\.clt\\.d\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_less\n" } } */
/* { dg-final { scan-assembler "compare_quiet_less_equal:.*\tvfcmp\\.cle\\.d\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_less_equal\n" } } */
/* { dg-final { scan-assembler "compare_quiet_greater:.*\tvfcmp\\.clt\\.d\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_quiet_greater\n" } } */
/* { dg-final { scan-assembler "compare_quiet_greater_equal:.*\tvfcmp\\.cle\\.d\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_quiet_greater_equal\n" } } */
/* { dg-final { scan-assembler "compare_quiet_not_less:.*\tvfcmp\\.cule\\.d\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_quiet_not_less\n" } } */
/* { dg-final { scan-assembler "compare_quiet_greater_unordered:.*\tvfcmp\\.cult\\.d\t\\\$vr2,\\\$vr1,\\\$vr0.*-compare_quiet_greater_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_not_greater:.*\tvfcmp\\.cule\\.d\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_not_greater\n" } } */
/* { dg-final { scan-assembler "compare_quiet_less_unordered:.*\tvfcmp\\.cult\\.d\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_less_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_unordered:.*\tvfcmp\\.cun\\.d\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_ordered:.*\tvfcmp\\.cor\\.d\t\\\$vr2,\\\$vr0,\\\$vr1.*-compare_quiet_ordered\n" } } */
