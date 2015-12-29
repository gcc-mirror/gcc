/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-sse -mskip-rax-setup" } */
/* { dg-final { scan-assembler-not "xorl\[\\t \]*\\\%eax,\[\\t \]*%eax" } } */

void foo (const char *, ...);

void
test1 (void)
{
  foo ("%d", 20);
}

int
test2 (void)
{
  foo ("%d", 20);
  return 3;
}
