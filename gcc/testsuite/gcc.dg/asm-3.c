/* { dg-do compile } */
/* { dg-options "" } */

int f()
{
  asm ("foo%%bar");
}

/* { dg-final { scan-assembler asm-3.c "foo%%bar" } } */
