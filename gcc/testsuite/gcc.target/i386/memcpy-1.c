/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-O2 -march=pentiumpro -minline-all-stringops -fno-common" } */
/* { dg-final { scan-assembler "rep" } } */
/* { dg-final { scan-assembler "movs" } } */
/* { dg-final { scan-assembler-not "test" } } */
/* { dg-final { scan-assembler "\.L?:" } } */

/* A and B are aligned, but we used to lose track of it.
   Ensure that memcpy is inlined and alignment prologue is missing.  */

char a[2048];
char b[2048];
t()
{
  __builtin_memcpy (a,b,2048);
}
