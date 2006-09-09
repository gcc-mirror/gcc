/* { dg-do compile { target i?86-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -march=pentiumpro -minline-all-stringops" } */
/* { dg-final { scan-assembler "rep" } } */
/* { dg-final { scan-assembler "movs" } } */
/* { dg-final { scan-assembler-not "test" } } */
/* { dg-final { scan-assembler "\.L?:" } } */

/* A and B are aligned, but we used to lose track of it.
   Ensure that memcpy is inlined and alignment prologue is missing.  */

char a[900];
char b[900];
t()
{
  __builtin_memcpy (a,b,900);
}
