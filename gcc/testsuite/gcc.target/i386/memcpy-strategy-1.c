/* { dg-do compile } */
/* { dg-options "-O2 -march=atom -mmemcpy-strategy=vector_loop:-1:align" } */
/* { dg-final { scan-assembler-times "movdqa" 8 { target { ! { ia32 } } } } } */
/* { dg-final { scan-assembler-times "movdqa" 4 { target { ia32 } } } } */

char a[2048];
char b[2048];
void t (void)
{
  __builtin_memcpy (a, b, 2048);
}

