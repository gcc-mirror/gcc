foo (int *p, int c)
{
  int a, b;
  a = p[0];
  b = p[1];
  c = p[2];
  if (b == 0)
    goto foo1;
  if (b < 0)
    goto foo2;;

  return a + b + c;
 foo1:
  return 1;
 foo2:
  return 2;
}
