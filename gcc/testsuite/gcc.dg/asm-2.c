/* { dg-do compile } */
/* { dg-options "" } */

int f()
{
  asm volatile ("foo%%bar" : : );
}

/* { dg-final { scan-assembler asm-2.c "foo%bar" } } */
