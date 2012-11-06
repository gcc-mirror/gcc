/* { dg-do assemble } */
/* { dg-options "-O2 -fno-align-functions -mtraceback=no -save-temps" } */

void w1 (void *x, double y) { *(double *) (x + 32767) = y; }
void w2 (void *x, double y) { *(double *) (x + 32766) = y; }
void w3 (void *x, double y) { *(double *) (x + 32765) = y; }
void w4 (void *x, double y) { *(double *) (x + 32764) = y; }
void w5 (void *x, double y) { *(double *) (x + 32763) = y; }
void w6 (void *x, double y) { *(double *) (x + 32762) = y; }
void w7 (void *x, double y) { *(double *) (x + 32761) = y; }
void w8 (void *x, double y) { *(double *) (x + 32760) = y; }
void w9 (void *x, double y) { *(double *) (x + 32759) = y; }
void w10 (void *x, double y) { *(double *) (x + 32758) = y; }
void w11 (void *x, double y) { *(double *) (x + 32757) = y; }
void w12 (void *x, double y) { *(double *) (x + 32756) = y; }
void w13 (void *x, double y) { *(double *) (x + 32755) = y; }
void w14 (void *x, double y) { *(double *) (x + 32754) = y; }
void w15 (void *x, double y) { *(double *) (x + 32753) = y; }
void w16 (void *x, double y) { *(double *) (x + 32752) = y; }
void w17 (void *x, double y) { *(double *) (x + 32751) = y; }
void w18 (void *x, double y) { *(double *) (x + 32750) = y; }
void w19 (void *x, double y) { *(double *) (x + 32749) = y; }
void w20 (void *x, double y) { *(double *) (x + 32748) = y; }

double r1 (void *x) { return *(double *) (x + 32767); }
double r2 (void *x) { return *(double *) (x + 32766); }
double r3 (void *x) { return *(double *) (x + 32765); }
double r4 (void *x) { return *(double *) (x + 32764); }
double r5 (void *x) { return *(double *) (x + 32763); }
double r6 (void *x) { return *(double *) (x + 32762); }
double r7 (void *x) { return *(double *) (x + 32761); }
double r8 (void *x) { return *(double *) (x + 32760); }
double r9 (void *x) { return *(double *) (x + 32759); }
double r10 (void *x) { return *(double *) (x + 32758); }
double r11 (void *x) { return *(double *) (x + 32757); }
double r12 (void *x) { return *(double *) (x + 32756); }
double r13 (void *x) { return *(double *) (x + 32755); }
double r14 (void *x) { return *(double *) (x + 32754); }
double r15 (void *x) { return *(double *) (x + 32753); }
double r16 (void *x) { return *(double *) (x + 32752); }
double r17 (void *x) { return *(double *) (x + 32751); }
double r18 (void *x) { return *(double *) (x + 32750); }
double r19 (void *x) { return *(double *) (x + 32749); }
double r20 (void *x) { return *(double *) (x + 32748); }

/* { dg-final { object-size text == 320 } } */
/* { dg-final { cleanup-saved-temps "dfmode_off" } } */
