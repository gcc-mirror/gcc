/* { dg-do compile } */
/* { dg-options "" } */

int f()
{
  asm ("foo%%bar");
}

/* { dg-final { scan-assembler "foo%%bar" } } */
