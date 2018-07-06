/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6m_ok } */
/* { dg-add-options arm_arch_v6m } */
/* { dg-additional-options "-O2" } */
/* { dg-final { scan-assembler-not "orr\[ \t\]" } } */
/* { dg-final { scan-assembler-times "revsh\\t" 2  } }  */
/* { dg-final { scan-assembler-times "rev16\\t" 2 } }  */
/* { dg-final { scan-assembler-times "rev\\t" 4 } }  */

#include "builtin-bswap.x"
