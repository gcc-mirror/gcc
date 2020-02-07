/* { dg-do compile } */
/* { dg-options "-O2 -march=atom -mmemcpy-strategy=vector_loop:2000:align,libcall:-1:align" } */
/* { dg-final { scan-assembler-times "call\[\\t \]*_?memcpy" 1 } } */

char a[2048];
char b[2048];
void t (void)
{
  __builtin_memcpy (a, b, 2048);
}
