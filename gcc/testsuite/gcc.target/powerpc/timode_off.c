/* { dg-do assemble { target { lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power5" } } */
/* { dg-options "-O2 -fno-align-functions -mtraceback=no -save-temps -mcpu=power5" } */

typedef int TImode __attribute__ ((mode (TI)));

void w1 (void *x, TImode y) { *(TImode *) (x + 32767) = y; }
void w2 (void *x, TImode y) { *(TImode *) (x + 32766) = y; }
void w3 (void *x, TImode y) { *(TImode *) (x + 32765) = y; }
void w4 (void *x, TImode y) { *(TImode *) (x + 32764) = y; }
void w5 (void *x, TImode y) { *(TImode *) (x + 32763) = y; }
void w6 (void *x, TImode y) { *(TImode *) (x + 32762) = y; }
void w7 (void *x, TImode y) { *(TImode *) (x + 32761) = y; }
void w8 (void *x, TImode y) { *(TImode *) (x + 32760) = y; }
void w9 (void *x, TImode y) { *(TImode *) (x + 32759) = y; }
void w10 (void *x, TImode y) { *(TImode *) (x + 32758) = y; }
void w11 (void *x, TImode y) { *(TImode *) (x + 32757) = y; }
void w12 (void *x, TImode y) { *(TImode *) (x + 32756) = y; }
void w13 (void *x, TImode y) { *(TImode *) (x + 32755) = y; }
void w14 (void *x, TImode y) { *(TImode *) (x + 32754) = y; }
void w15 (void *x, TImode y) { *(TImode *) (x + 32753) = y; }
void w16 (void *x, TImode y) { *(TImode *) (x + 32752) = y; }
void w17 (void *x, TImode y) { *(TImode *) (x + 32751) = y; }
void w18 (void *x, TImode y) { *(TImode *) (x + 32750) = y; }
void w19 (void *x, TImode y) { *(TImode *) (x + 32749) = y; }
void w20 (void *x, TImode y) { *(TImode *) (x + 32748) = y; }

TImode r1 (void *x) { return *(TImode *) (x + 32767); }
TImode r2 (void *x) { return *(TImode *) (x + 32766); }
TImode r3 (void *x) { return *(TImode *) (x + 32765); }
TImode r4 (void *x) { return *(TImode *) (x + 32764); }
TImode r5 (void *x) { return *(TImode *) (x + 32763); }
TImode r6 (void *x) { return *(TImode *) (x + 32762); }
TImode r7 (void *x) { return *(TImode *) (x + 32761); }
TImode r8 (void *x) { return *(TImode *) (x + 32760); }
TImode r9 (void *x) { return *(TImode *) (x + 32759); }
TImode r10 (void *x) { return *(TImode *) (x + 32758); }
TImode r11 (void *x) { return *(TImode *) (x + 32757); }
TImode r12 (void *x) { return *(TImode *) (x + 32756); }
TImode r13 (void *x) { return *(TImode *) (x + 32755); }
TImode r14 (void *x) { return *(TImode *) (x + 32754); }
TImode r15 (void *x) { return *(TImode *) (x + 32753); }
TImode r16 (void *x) { return *(TImode *) (x + 32752); }
TImode r17 (void *x) { return *(TImode *) (x + 32751); }
TImode r18 (void *x) { return *(TImode *) (x + 32750); }
TImode r19 (void *x) { return *(TImode *) (x + 32749); }
TImode r20 (void *x) { return *(TImode *) (x + 32748); }

/* test should really be == 616, see pr54110 */
/* When TImode is allowed in VSX registers, the allowable address modes for
   TImode is just a single indirect address in order for the value to be loaded
   and store in either GPR or VSX registers.  This affects the generated code,
   and it would cause this test to fail, when such an option is used.  Fall
   back to power5 to test the code.  */

/* { dg-final { object-size text <= 700 } } */
/* { dg-final { scan-assembler-not "(st|l)fd" } } */
