/* The goal here is to verify that increasing the size of the guard allows
   elimination of all probing on the relevant targets.  */
   
/* { dg-do compile  } */
/* { dg-options "-O2 -fstack-clash-protection -fdump-rtl-pro_and_epilogue -fno-optimize-sibling-calls --param stack-clash-protection-probe-interval=12 --param stack-clash-protection-guard-size=16" } */
/* { dg-require-effective-target supports_stack_clash_protection  } */
/* { dg-skip-if "" { *-*-* } { "-fstack-protector*" } { "" } } */
/* { dg-skip-if "" { riscv*-*-* aarch64*-*-* } } */


#include "stack-check-6.c"

/* { dg-final { scan-rtl-dump-times "Stack clash inline probes" 0 "pro_and_epilogue" } } */
/* { dg-final { scan-rtl-dump-times "Stack clash probe loop" 0 "pro_and_epilogue" } } */
/* { dg-final { scan-rtl-dump-times "Stack clash residual allocation in prologue" 4 "pro_and_epilogue" } } */
/* { dg-final { scan-rtl-dump-times "Stack clash not noreturn" 4 "pro_and_epilogue" } } */

/* { dg-final { scan-rtl-dump-times "Stack clash no frame pointer needed" 4 "pro_and_epilogue" { target { ! frame_pointer_for_non_leaf } } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash no frame pointer needed" 2 "pro_and_epilogue" { target { frame_pointer_for_non_leaf } } } } */
/* { dg-final { scan-rtl-dump-times "Stack clash frame pointer needed" 2 "pro_and_epilogue" { target { frame_pointer_for_non_leaf } } } } */
