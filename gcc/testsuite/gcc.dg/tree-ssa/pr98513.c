/* { dg-do run } */
/* { dg-options "-O2 -fgimple" } */

__attribute__((noipa))
void __GIMPLE (ssa,startwith("evrp"))
foo (int x, int minus_1)
{
  int tem;
  unsigned int _1;
  unsigned int _2;

  __BB(2):
  tem_4 = minus_1_3(D);
  tem_5 = tem_4 + 2;
  _1 = (unsigned int) x_6(D);
  _2 = _1 + 2147483647u;
  if (_2 > 1u)
    goto __BB3;
  else
    goto __BB6;

  __BB(3):
  if (x_6(D) <= tem_5)
    goto __BB4;
  else
    goto __BB6;

  __BB(4):
  if (x_6(D) > 5)
    goto __BB5;
  else
    goto __BB6;

  __BB(5):
  __builtin_exit (0);

  __BB(6):
  return;

}

int
main()
{
  foo (10, 100);
  __builtin_abort ();
}
