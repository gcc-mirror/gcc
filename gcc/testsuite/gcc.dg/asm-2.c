/* { dg-do compile } */
/* { dg-options "" } */

int f()
{
  asm volatile ("foo%%bar" : : );
}

/* { dg-final { scan-assembler "foo%bar" } } */
