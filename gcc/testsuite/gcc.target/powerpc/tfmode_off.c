/* { dg-do assemble } */
/* { dg-skip-if "" { powerpc-ibm-aix* } { "*" } { "" } } */
/* { dg-skip-if "no TFmode" { powerpc-*-eabi* } { "*" } { "" } } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-options "-O2 -fno-align-functions -mtraceback=no -save-temps" } */

typedef float TFmode __attribute__ ((mode (TF)));

void w1 (void *x, TFmode y) { *(TFmode *) (x + 32767) = y; }
void w2 (void *x, TFmode y) { *(TFmode *) (x + 32766) = y; }
void w3 (void *x, TFmode y) { *(TFmode *) (x + 32765) = y; }
void w4 (void *x, TFmode y) { *(TFmode *) (x + 32764) = y; }
void w5 (void *x, TFmode y) { *(TFmode *) (x + 32763) = y; }
void w6 (void *x, TFmode y) { *(TFmode *) (x + 32762) = y; }
void w7 (void *x, TFmode y) { *(TFmode *) (x + 32761) = y; }
void w8 (void *x, TFmode y) { *(TFmode *) (x + 32760) = y; }
void w9 (void *x, TFmode y) { *(TFmode *) (x + 32759) = y; }
void w10 (void *x, TFmode y) { *(TFmode *) (x + 32758) = y; }
void w11 (void *x, TFmode y) { *(TFmode *) (x + 32757) = y; }
void w12 (void *x, TFmode y) { *(TFmode *) (x + 32756) = y; }
void w13 (void *x, TFmode y) { *(TFmode *) (x + 32755) = y; }
void w14 (void *x, TFmode y) { *(TFmode *) (x + 32754) = y; }
void w15 (void *x, TFmode y) { *(TFmode *) (x + 32753) = y; }
void w16 (void *x, TFmode y) { *(TFmode *) (x + 32752) = y; }
void w17 (void *x, TFmode y) { *(TFmode *) (x + 32751) = y; }
void w18 (void *x, TFmode y) { *(TFmode *) (x + 32750) = y; }
void w19 (void *x, TFmode y) { *(TFmode *) (x + 32749) = y; }
void w20 (void *x, TFmode y) { *(TFmode *) (x + 32748) = y; }

TFmode r1 (void *x) { return *(TFmode *) (x + 32767); }
TFmode r2 (void *x) { return *(TFmode *) (x + 32766); }
TFmode r3 (void *x) { return *(TFmode *) (x + 32765); }
TFmode r4 (void *x) { return *(TFmode *) (x + 32764); }
TFmode r5 (void *x) { return *(TFmode *) (x + 32763); }
TFmode r6 (void *x) { return *(TFmode *) (x + 32762); }
TFmode r7 (void *x) { return *(TFmode *) (x + 32761); }
TFmode r8 (void *x) { return *(TFmode *) (x + 32760); }
TFmode r9 (void *x) { return *(TFmode *) (x + 32759); }
TFmode r10 (void *x) { return *(TFmode *) (x + 32758); }
TFmode r11 (void *x) { return *(TFmode *) (x + 32757); }
TFmode r12 (void *x) { return *(TFmode *) (x + 32756); }
TFmode r13 (void *x) { return *(TFmode *) (x + 32755); }
TFmode r14 (void *x) { return *(TFmode *) (x + 32754); }
TFmode r15 (void *x) { return *(TFmode *) (x + 32753); }
TFmode r16 (void *x) { return *(TFmode *) (x + 32752); }
TFmode r17 (void *x) { return *(TFmode *) (x + 32751); }
TFmode r18 (void *x) { return *(TFmode *) (x + 32750); }
TFmode r19 (void *x) { return *(TFmode *) (x + 32749); }
TFmode r20 (void *x) { return *(TFmode *) (x + 32748); }

/* { dg-final { object-size text == 544 } } */
