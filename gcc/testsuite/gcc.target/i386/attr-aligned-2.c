/* Verify that valid alignment on either a function declaration
   or a definition has the expected effect and overrides -Os.
   { dg-do compile }
   { dg-options "-Os" }  */

#define ALIGN(n)         __attribute__ ((aligned (n)))

/* No alignment specified (to cause the subsequent instruction
   to be at an odd boundary due to -Os).  */
void f (void) { }

void f4 (void);

ALIGN (4)
void f4 (void) { }

/* { dg-final { scan-assembler ".align 4\n\t.globl\tf4" { target { ! *-*-darwin* } } } } */
/* { dg-final { scan-assembler {.align[ \t]2,0x90\n\t.globl[ \t]_f4} { target *-*-darwin*  } } } */

void g (void) { }


ALIGN (4)
void g4 (void);

void g4 (void) { }
