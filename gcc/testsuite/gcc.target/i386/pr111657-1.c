/* { dg-do assemble } */
/* { dg-options "-O2 -mno-sse -mtune=generic -save-temps" } */

typedef unsigned long uword __attribute__ ((mode (word)));

struct a { uword arr[30]; };

__seg_gs struct a m;
void bar (struct a *dst) { *dst = m; }

/* { dg-final { scan-assembler "gs\[ \t\]+rep\[; \t\]+movs(l|q)" { target { ! x32 } } } } */
/* { dg-final { scan-assembler-not "gs\[ \t\]+rep\[; \t\]+movs(l|q)" { target x32 } } } */
