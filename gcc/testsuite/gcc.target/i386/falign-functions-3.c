/* Verify that attribute aligned overrides the effect of -falign-functions.
   (But see PR 88345 showing that -Os overrides -falign-functions.)
   The test may need to be adjusted if/when GCC implements PR 88231.
  { dg-do compile }
  { dg-options "-O2 -falign-functions=32" } */

#define ALIGN(n)         __attribute__ ((aligned (n)))

ALIGN (4)
void f4 (void) { }

/* { dg-final { scan-assembler ".align 4\n\t.globl\tf4" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler {.align 2,0x90\n\t.globl[ \t]_f4} { target *-*-darwin* } } } */

void f32 (void) { }

/* { dg-final { scan-assembler {.p2align 5\n\t.globl[ \t]_?f32} } } */


ALIGN (64)
void f64 (void) { }

/* { dg-final { scan-assembler ".align 64\n\t.globl\tf64" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler {.align 6,0x90\n\t.globl[ \t]_f64} { target *-*-darwin* } } } */
