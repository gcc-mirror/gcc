/* { dg-do compile } */
/* { dg-options "" } */

void f()
{
  asm volatile ("foo%%bar" : : );
}

/* { dg-final { scan-assembler "foo%bar" } } */
