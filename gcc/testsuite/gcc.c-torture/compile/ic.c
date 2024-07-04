int
foo (int *ip, int a)
{
  a++;
  if (a < ip[a])
    return 1;
  return 0;
}
