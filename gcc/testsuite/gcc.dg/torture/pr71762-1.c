/* { dg-do run } */
/* { dg-additional-options "-fdisable-rtl-init-regs" } */

static _Bool
foo (_Bool a, _Bool b)
{
  int x = a && ! b;
  return x != 0;
}

int y = 1;
int main()
{
  _Bool x;
  if (foo (x, y))
    __builtin_abort ();
  return 0;
}
