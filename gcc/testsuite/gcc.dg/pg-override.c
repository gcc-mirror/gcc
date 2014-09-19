/* Test -fprofile override */
/* { dg-do compile } */
/* { dg-options "-fprofile" { target i?86-*-linux* x86_64-*-linux* } } */
/* { dg-final { scan-assembler-not "mcount" } } */
/* Origin: Andi Kleen */
extern void foobar(const char *);

__attribute__((no_instrument_function)) void func(void)
{
  foobar ("Hello world\n");
}

__attribute__((no_instrument_function)) void func2(void)
{
  int i;
  for (i = 0; i < 10; i++)
    foobar ("Hello world");
}
