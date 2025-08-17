/* { dg-options "-funroll-loops isa_rev>=2" } */
int foo;
int bar;

NOMIPS16 void __attribute__ ((interrupt))
isr (void)
{
  if (!foo)
    while (bar & 0xFF30);
}
/* { dg-final { scan-assembler-not "^isr:.*\\\$8.*isr" } } */
