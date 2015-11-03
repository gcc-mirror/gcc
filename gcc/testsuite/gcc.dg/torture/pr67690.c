/* { dg-do run } */

const int c1 = 1;
const int c2 = 2;

int
check (int i)
{
  int j;
  if (i >= 0)
    j = c2 - i;
  else
    j = c2 - i;
  return c2 - c1 + 1 > j;
}

int invoke (int *pi) __attribute__ ((noinline,noclone));
int
invoke (int *pi)
{
  return check (*pi);
}

int
main ()
{
  int i = c1;
  int ret = invoke (&i);
  if (!ret)
    __builtin_abort ();
  return 0;
}
