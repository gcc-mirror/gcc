/* { dg-do compile } */

int
test5_limit (void)
{
  int addr;

  asm goto ("" : "+r" (addr) : : : t_err);
  return 0;

 t_err:
  return 1;
}
