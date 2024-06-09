/* { dg-do run } */
/* { dg-options "-Os -save-temps -fdata-sections -fcommon" } */

#include "attribute-io.h"

/* { dg-final { scan-assembler "g_1234 = 1234" } } */
/* { dg-final { scan-assembler "w_4321 = 4321" } } */
/* { dg-final { scan-assembler "l_5678 = 5678" } } */

/* { dg-final { scan-assembler "\\.globl	g_1234" } } */
/* { dg-final { scan-assembler "\\.globl	g_low" } } */
/* { dg-final { scan-assembler "\\.globl	g_io" } } */

/* { dg-final { scan-assembler "\\.weak	w_4321" } } */
/* { dg-final { scan-assembler "\\.weak	w_low" } } */
/* { dg-final { scan-assembler "\\.weak	w_io" } } */
