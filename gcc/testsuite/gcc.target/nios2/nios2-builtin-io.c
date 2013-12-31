/* { dg-do compile } */
/* { dg-final { scan-assembler "ldbio" } } */
/* { dg-final { scan-assembler "ldbuio" } } */
/* { dg-final { scan-assembler "ldhio" } } */
/* { dg-final { scan-assembler "ldhuio" } } */
/* { dg-final { scan-assembler "ldwio" } } */
/* { dg-final { scan-assembler "stbio" } } */
/* { dg-final { scan-assembler "sthio" } } */
/* { dg-final { scan-assembler "stwio" } } */

volatile char b;
volatile short h;
volatile int w;

void x ()
{
  __builtin_ldbio (&b);
  __builtin_ldbuio (&b);
  __builtin_ldhio (&h);
  __builtin_ldhuio (&h);
  __builtin_ldwio (&w);

  __builtin_stbio (&b, 42);
  __builtin_sthio (&h, 43);
  __builtin_stwio (&w, 44);
} 
