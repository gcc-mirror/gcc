/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6t2_ok } */
/* { dg-add-options arm_arch_v6t2 } */
/* { dg-additional-options "-O2" } */
/* { dg-final { scan-assembler-not "orr\[ \t\]" } } */

#include "builtin-bswap16.x"
