/* { dg-do compile } */
/* { dg-options "-O0 -fdump-rtl-expand" } */

extern void foo(void);

void BGEUI_test(unsigned int a)
{
  if (a < 32768U)
    foo();
}

void BLTUI_test(unsigned int a)
{
  if (a >= 65536U)
    foo();
}

/* { dg-final { scan-rtl-dump-times "ubtrue" 2 "expand" } } */
/* { dg-final { scan-assembler-times "bgeui" 1 } } */
/* { dg-final { scan-assembler-times "bltui" 1 } } */
