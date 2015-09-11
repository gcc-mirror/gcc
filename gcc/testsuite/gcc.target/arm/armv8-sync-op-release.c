/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_arch_v8a } */

#include "../aarch64/sync-op-release.x"

/* { dg-final { scan-assembler-times "stl" 1 } } */
