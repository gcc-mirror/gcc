/* { dg-do compile } */
/* { dg-require-alias "" } */

int yum;
void dessert (void) { ++yum; }
extern void jelly (void) __attribute__ ((alias ("dessert"), weak));
extern void wobbly (void) __attribute__ ((alias ("jelly"), weak));

/* { dg-final { scan-assembler "wobbly" } } */
/* { dg-final { scan-assembler "jelly" } } */
