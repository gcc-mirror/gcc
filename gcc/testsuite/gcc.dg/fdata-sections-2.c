/* PR middle-end/192 */
/* PR middle-end/54303 */

/* This checks that string constants are put in per-function rodata
   sections, so that they can be garbage collected.  */

/* { dg-do compile { target *-*-linux* *-*-uclinux* } } */
/* { dg-options "-O -ffunction-sections -fdata-sections" } */

const char *f1(void) { return "falderalde"; }
const char *f2(void) { return "a"; }
const char *f3(void) { return "falderalde"; }
const char *f4(void) { return "eralde"; }

/* { dg-final { scan-assembler {\.rodata\.f1\.str} } } */
/* { dg-final { scan-assembler {\.rodata\.f2\.str} } } */
/* { dg-final { scan-assembler-not {\.rodata\.f3\.str} } } */
/* { dg-final { scan-assembler {\.rodata\.f4\.str} } } */
