/* { dg-do compile } */
/* { dg-options "" } */

void f()
{
  asm ("foo%%bar");
}

/* { dg-final { scan-assembler "foo%%bar" } } */
