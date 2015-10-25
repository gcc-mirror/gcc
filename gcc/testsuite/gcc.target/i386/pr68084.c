/* { dg-do compile } */
/* { dg-options "-O" } */

int x;

void foo (void)
{
  char r;

  asm ("" : "=@ccae"(r));

  if (!r)
    x = 0;
}

/* { dg-final { scan-assembler "jnc" } } */
