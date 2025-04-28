/* { dg-do compile { target x86_64-*-* } } */

void *
get_from_hard_reg (void)
{
  register void *sp asm ("sp");
  return sp;
}
