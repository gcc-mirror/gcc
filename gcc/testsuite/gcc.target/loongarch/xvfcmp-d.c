/* { dg-do compile } */
/* { dg-options "-O2 -mlasx -ffixed-f0 -ffixed-f1 -ffixed-f2 -fno-vect-cost-model" } */

#define F double
#define I long long
#define VL 32

#include "vfcmp-f.c"

/* { dg-final { scan-assembler "compare_quiet_equal:.*\txvfcmp\\.ceq\\.d\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_equal\n" } } */
/* { dg-final { scan-assembler "compare_quiet_not_equal:.*\txvfcmp\\.cune\\.d\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_not_equal\n" } } */
/* { dg-final { scan-assembler "compare_signaling_greater:.*\txvfcmp\\.slt\\.d\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_signaling_greater\n" } } */
/* { dg-final { scan-assembler "compare_signaling_greater_equal:.*\txvfcmp\\.sle\\.d\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_signaling_greater_equal\n" } } */
/* { dg-final { scan-assembler "compare_signaling_less:.*\txvfcmp\\.slt\\.d\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_signaling_less\n" } } */
/* { dg-final { scan-assembler "compare_signaling_less_equal:.*\txvfcmp\\.sle\\.d\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_signaling_less_equal\n" } } */
/* { dg-final { scan-assembler "compare_signaling_not_greater:.*\txvfcmp\\.sule\\.d\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_signaling_not_greater\n" } } */
/* { dg-final { scan-assembler "compare_signaling_less_unordered:.*\txvfcmp\\.sult\\.d\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_signaling_less_unordered\n" } } */
/* { dg-final { scan-assembler "compare_signaling_not_less:.*\txvfcmp\\.sule\\.d\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_signaling_not_less\n" } } */
/* { dg-final { scan-assembler "compare_signaling_greater_unordered:.*\txvfcmp\\.sult\\.d\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_signaling_greater_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_less:.*\txvfcmp\\.clt\\.d\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_less\n" } } */
/* { dg-final { scan-assembler "compare_quiet_less_equal:.*\txvfcmp\\.cle\\.d\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_less_equal\n" } } */
/* { dg-final { scan-assembler "compare_quiet_greater:.*\txvfcmp\\.clt\\.d\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_quiet_greater\n" } } */
/* { dg-final { scan-assembler "compare_quiet_greater_equal:.*\txvfcmp\\.cle\\.d\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_quiet_greater_equal\n" } } */
/* { dg-final { scan-assembler "compare_quiet_not_less:.*\txvfcmp\\.cule\\.d\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_quiet_not_less\n" } } */
/* { dg-final { scan-assembler "compare_quiet_greater_unordered:.*\txvfcmp\\.cult\\.d\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_quiet_greater_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_not_greater:.*\txvfcmp\\.cule\\.d\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_not_greater\n" } } */
/* { dg-final { scan-assembler "compare_quiet_less_unordered:.*\txvfcmp\\.cult\\.d\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_less_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_unordered:.*\txvfcmp\\.cun\\.d\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_ordered:.*\txvfcmp\\.cor\\.d\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_ordered\n" } } */
