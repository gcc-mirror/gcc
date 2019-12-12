/* Test -mfentry */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-require-effective-target mfentry } */
/* { dg-options "-fprofile -mfentry" } */
/* { dg-final { scan-assembler "__fentry__" } } */
/* Origin: Andi Kleen */
extern void foobar(const char *);

void func(void)
{
  foobar ("Hello world\n");
}

void func2(void)
{
  int i;
  for (i = 0; i < 10; i++)
    foobar ("Hello world");
}
