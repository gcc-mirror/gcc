/* { dg-do compile } */
/* { dg-require-effective-target arm_hard_vfp_ok }  */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-options "-march=armv7-a -mfpu=vfp -fstack-protector-all -Os -mstack-protector-guard=tls -mstack-protector-guard-offset=1296 -mtp=cp15" } */

#include "stack-protector-5.c"

/* See the comment in stack-protector-5.c.  */
/* { dg-final { scan-assembler-times {\tstr\t} 1 } } */
/* Expect two TLS register accesses and two occurrences of the offset */
/* { dg-final { scan-assembler-times {\tmrc\t} 2 } } */
/* { dg-final { scan-assembler-times {1296} 2 } } */
