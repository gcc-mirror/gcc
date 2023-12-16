/* { dg-do compile } */
/* { dg-options "-O2 -mlasx -ffixed-f0 -ffixed-f1 -ffixed-f2" } */

#define VL 32

#include "vfcmp-f.c"

/* { dg-final { scan-assembler "compare_quiet_equal:.*\txvfcmp\\.ceq\\.s\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_equal\n" } } */
/* { dg-final { scan-assembler "compare_quiet_not_equal:.*\txvfcmp\\.cune\\.s\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_not_equal\n" } } */
/* { dg-final { scan-assembler "compare_signaling_greater:.*\txvfcmp\\.slt\\.s\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_signaling_greater\n" } } */
/* { dg-final { scan-assembler "compare_signaling_greater_equal:.*\txvfcmp\\.sle\\.s\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_signaling_greater_equal\n" } } */
/* { dg-final { scan-assembler "compare_signaling_less:.*\txvfcmp\\.slt\\.s\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_signaling_less\n" } } */
/* { dg-final { scan-assembler "compare_signaling_less_equal:.*\txvfcmp\\.sle\\.s\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_signaling_less_equal\n" } } */
/* { dg-final { scan-assembler "compare_signaling_not_greater:.*\txvfcmp\\.sule\\.s\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_signaling_not_greater\n" } } */
/* { dg-final { scan-assembler "compare_signaling_less_unordered:.*\txvfcmp\\.sult\\.s\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_signaling_less_unordered\n" } } */
/* { dg-final { scan-assembler "compare_signaling_not_less:.*\txvfcmp\\.sule\\.s\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_signaling_not_less\n" } } */
/* { dg-final { scan-assembler "compare_signaling_greater_unordered:.*\txvfcmp\\.sult\\.s\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_signaling_greater_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_less:.*\txvfcmp\\.clt\\.s\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_less\n" } } */
/* { dg-final { scan-assembler "compare_quiet_less_equal:.*\txvfcmp\\.cle\\.s\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_less_equal\n" } } */
/* { dg-final { scan-assembler "compare_quiet_greater:.*\txvfcmp\\.clt\\.s\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_quiet_greater\n" } } */
/* { dg-final { scan-assembler "compare_quiet_greater_equal:.*\txvfcmp\\.cle\\.s\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_quiet_greater_equal\n" } } */
/* { dg-final { scan-assembler "compare_quiet_not_less:.*\txvfcmp\\.cule\\.s\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_quiet_not_less\n" } } */
/* { dg-final { scan-assembler "compare_quiet_greater_unordered:.*\txvfcmp\\.cult\\.s\t\\\$xr2,\\\$xr1,\\\$xr0.*-compare_quiet_greater_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_not_greater:.*\txvfcmp\\.cule\\.s\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_not_greater\n" } } */
/* { dg-final { scan-assembler "compare_quiet_less_unordered:.*\txvfcmp\\.cult\\.s\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_less_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_unordered:.*\txvfcmp\\.cun\\.s\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_unordered\n" } } */
/* { dg-final { scan-assembler "compare_quiet_ordered:.*\txvfcmp\\.cor\\.s\t\\\$xr2,\\\$xr0,\\\$xr1.*-compare_quiet_ordered\n" } } */
