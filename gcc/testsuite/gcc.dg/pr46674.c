/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-options "-O2" } */

int yum;
void dessert (void) { ++yum; }
extern void jelly (void) __asm__ ("jelly2") __attribute__ ((alias ("dessert"), weak));
extern void wobbly (void) __attribute__ ((alias ("jelly2"), weak));

/* { dg-final { scan-assembler "wobbly" } } */
/* { dg-final { scan-assembler "jelly2" } } */
