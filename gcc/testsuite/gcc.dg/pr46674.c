/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-options "-O2" } */

#define LABEL3(pfx, x) # pfx x
#define LABEL2(pfx, x) LABEL3(pfx, x)
#define LABEL(x) LABEL2(__USER_LABEL_PREFIX__, x)

int yum;
void dessert (void) { ++yum; }
extern void jelly (void) __asm__ (LABEL ("jelly2")) __attribute__ ((alias ("dessert"), weak));
extern void wobbly (void) __attribute__ ((alias ("jelly2"), weak));

/* { dg-final { scan-assembler "wobbly" } } */
/* { dg-final { scan-assembler "jelly2" } } */
