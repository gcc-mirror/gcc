/* { dg-do compile } */
/* { dg-options "-O2 -mno-sse -mtune=generic -masm=att" } */

typedef unsigned long uword __attribute__ ((mode (word)));

struct a { uword arr[30]; };

__seg_gs struct a m;
void bar (struct a *dst) { *dst = m; }

/* { dg-final { scan-assembler "rep\[; \t\]+movs(l|q)\[ \t\]+%gs:" { target { ! x32 } } } } */
/* { dg-final { scan-assembler-not "rep\[; \t\]+movs(l|q)\[ \t\]+%gs:" { target x32 } } } */
