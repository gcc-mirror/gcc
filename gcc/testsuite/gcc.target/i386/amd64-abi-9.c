/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mno-sse -mno-skip-rax-setup" } */
/* { dg-final { scan-assembler-times "xorl\[\\t \]*\\\%eax,\[\\t \]*%eax" 2 } } */

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
