/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O0 -gdwarf-2" } */
/* { dg-final { scan-assembler "loc \[0-9] 9 \[0-9]( is_stmt \[0-9])?\n" } } */
/* { dg-final { scan-assembler "loc \[0-9] 9 \[0-9]( is_stmt \[0-9])? discriminator 2\n" } } */
/* { dg-final { scan-assembler "loc \[0-9] 9 \[0-9]( is_stmt \[0-9])? discriminator 1\n" } } */

int foo(int n) {
  int i, ret = 0;
  for (i = 0; i < n; i++) {
    if (i % 10 == 1)
      ret++;
    else
      ret--;
  }
  return ret;
}
