/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__ ((noipa)) static int
a ()
{
  return 0;
}

__attribute__ ((noipa)) static int
b ()
{
  return 1;
}

__attribute__ ((noipa)) static int
calc (int cond)
{
  int v = a ();
  if (cond)
    return b () && v;
  return v;
}

int
main ()
{
  int r = calc (1);
  if (r)
    __builtin_trap ();
}
