/* { dg-do compile } */
/* { dg-final { scan-assembler "add\tsp, sp, x5" } } */
/* { dg-final { scan-assembler "br\tx6" } } */

void
foo (unsigned long off, void *handler)
{
  __builtin_eh_return (off, handler);
}
