/* PR rtl-optimization/78132 */
/* { dg-do compile } */
/* { dg-options "-O3 -mavx512f -mno-avx512bw -mno-avx512dq -masm=att" } */
/* { dg-final { scan-assembler-not "kmov\[dq]\t" } } */

unsigned short c;
char a, d, f, b;
short e;
long g;

int
main ()
{
  g = c;
  f = c & e;
  d = c & a | e;
  if (a)
    b = c;
  return 0;
}
