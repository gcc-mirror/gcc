/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6m_ok } */
/* { dg-add-options arm_arch_v6m } */
/* { dg-additional-options "-O2" } */
/* { dg-final { scan-assembler-not "orr\[ \t\]" } } */

#include "builtin-bswap16.x"
