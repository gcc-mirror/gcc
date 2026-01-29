/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7a_fp_hard_ok } */
/* { dg-options "-fstack-protector-all -Os -mstack-protector-guard=tls -mstack-protector-guard-offset=1296 -mtp=cp15" } */
/* { dg-add-options arm_arch_v7a_fp_hard } */

#include "stack-protector-5.c"

/* See the comment in stack-protector-5.c.  */
/* { dg-final { scan-assembler-times {\tstr\t} 1 } } */
/* Expect two TLS register accesses and two occurrences of the offset */
/* { dg-final { scan-assembler-times {\tmrc\t} 2 } } */
/* { dg-final { scan-assembler-times {1296} 2 } } */
