foo (a, b)
{
  if (a < 0)
    goto ret1;
  if (a == 0)
    return 2;
  return 3;
 ret1:
  return 1;
}

