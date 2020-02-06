/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=atom" } } */
/* { dg-options "-O2 -march=atom -mmemcpy-strategy=vector_loop:3000:align,libcall:-1:align" } */
/* { dg-final { scan-assembler-times "movdqa" 8 } } */

char a[2048];
char b[2048];
void t (void)
{
  __builtin_memcpy (a, b, 2048);
}

