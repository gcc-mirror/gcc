/* { dg-do assemble } */
/* { dg-options "-O2 -fno-align-functions -fno-asynchronous-unwind-tables -mtraceback=no -mno-prefixed -save-temps" } */

void w1 (void *x, long long y) { *(long long *) (x + 32767) = y; }
void w2 (void *x, long long y) { *(long long *) (x + 32766) = y; }
void w3 (void *x, long long y) { *(long long *) (x + 32765) = y; }
void w4 (void *x, long long y) { *(long long *) (x + 32764) = y; }
void w5 (void *x, long long y) { *(long long *) (x + 32763) = y; }
void w6 (void *x, long long y) { *(long long *) (x + 32762) = y; }
void w7 (void *x, long long y) { *(long long *) (x + 32761) = y; }
void w8 (void *x, long long y) { *(long long *) (x + 32760) = y; }
void w9 (void *x, long long y) { *(long long *) (x + 32759) = y; }
void w10 (void *x, long long y) { *(long long *) (x + 32758) = y; }
void w11 (void *x, long long y) { *(long long *) (x + 32757) = y; }
void w12 (void *x, long long y) { *(long long *) (x + 32756) = y; }
void w13 (void *x, long long y) { *(long long *) (x + 32755) = y; }
void w14 (void *x, long long y) { *(long long *) (x + 32754) = y; }
void w15 (void *x, long long y) { *(long long *) (x + 32753) = y; }
void w16 (void *x, long long y) { *(long long *) (x + 32752) = y; }
void w17 (void *x, long long y) { *(long long *) (x + 32751) = y; }
void w18 (void *x, long long y) { *(long long *) (x + 32750) = y; }
void w19 (void *x, long long y) { *(long long *) (x + 32749) = y; }
void w20 (void *x, long long y) { *(long long *) (x + 32748) = y; }

long long r1 (void *x) { return *(long long *) (x + 32767); }
long long r2 (void *x) { return *(long long *) (x + 32766); }
long long r3 (void *x) { return *(long long *) (x + 32765); }
long long r4 (void *x) { return *(long long *) (x + 32764); }
long long r5 (void *x) { return *(long long *) (x + 32763); }
long long r6 (void *x) { return *(long long *) (x + 32762); }
long long r7 (void *x) { return *(long long *) (x + 32761); }
long long r8 (void *x) { return *(long long *) (x + 32760); }
long long r9 (void *x) { return *(long long *) (x + 32759); }
long long r10 (void *x) { return *(long long *) (x + 32758); }
long long r11 (void *x) { return *(long long *) (x + 32757); }
long long r12 (void *x) { return *(long long *) (x + 32756); }
long long r13 (void *x) { return *(long long *) (x + 32755); }
long long r14 (void *x) { return *(long long *) (x + 32754); }
long long r15 (void *x) { return *(long long *) (x + 32753); }
long long r16 (void *x) { return *(long long *) (x + 32752); }
long long r17 (void *x) { return *(long long *) (x + 32751); }
long long r18 (void *x) { return *(long long *) (x + 32750); }
long long r19 (void *x) { return *(long long *) (x + 32749); }
long long r20 (void *x) { return *(long long *) (x + 32748); }

/* { dg-final { object-size text == 440 { target { lp64 } } } } */
/* 32-bit test should really be == 512 bytes, see pr54110 */
/* { dg-final { object-size text <= 640 { target { ilp32 } } } } */
/* { dg-final { scan-assembler-not "(st|l)fd" } } */
