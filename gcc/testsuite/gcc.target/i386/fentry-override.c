/* Test -mfentry override */
/* { dg-do compile { target { { *-*-linux* } && { ! ia32 } } } } */
/* { dg-options "-mfentry" } */
/* { dg-final { scan-assembler-not "__fentry__" } } */
/* Origin: Andi Kleen */
extern void foobar(const char *);

void __attribute__((no_instrument_function)) func(void)
{
  foobar ("Hello world\n");
}

void __attribute__((no_instrument_function)) func2(void)
{
  int i;
  for (i = 0; i < 10; i++)
    foobar ("Hello world");
}
