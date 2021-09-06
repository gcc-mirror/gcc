/* PR target/95798 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vectorize -masm=att -fomit-frame-pointer" } */
/* { dg-final { scan-assembler "1, 8\\\(%rsp,%r\[a-z0-9]*,8\\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "2, 16\\\(%rsp,%r\[a-z0-9]*,8\\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "3, 24\\\(%rsp,%r\[a-z0-9]*,8\\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "4, 32\\\(%rsp,%r\[a-z0-9]*,8\\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "5, 40\\\(%rsp,%r\[a-z0-9]*,8\\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "6, 48\\\(%rsp,%r\[a-z0-9]*,8\\\)" { target lp64 } } } */
/* { dg-final { scan-assembler "7, 56\\\(%rsp,%r\[a-z0-9]*,8\\\)" { target lp64 } } } */

void bar (unsigned long long *, int);

void
foo (unsigned int y, unsigned long long z)
{
  unsigned long long x[1024];
  unsigned long long i = y % 127;
  __builtin_memset (x, -1, sizeof (x));
  x[i] = 0;
  x[i + 1] = 1;
  x[i + 2] = 2;
  x[i + 3] = 3;
  x[i + 4] = 4;
  x[i + 5] = 5;
  x[i + 6] = 6;
  x[i + 7] = 7;
  bar (x, y);
}
