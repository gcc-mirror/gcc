/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution" } */

void
caml_interprete (void)
{
  register int *pc asm("%r15");
  register int *sp asm("%r14");
  int i;

  for (i = 0; i < 3; ++i)
    *--sp = pc[i];
}
