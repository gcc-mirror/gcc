/* { dg-do run } */

static _Bool
foo (_Bool a, _Bool b)
{
  int x = a && ! b;
  return x != 0;
}

int y = 1;
int main()
{
  _Bool x[32];
  if (foo (x[1], y))
    __builtin_abort ();
  return 0;
}
