BUG2 (p) int *p;
{
  int a = 0;
  if (*p == a)
    return 0;
  else
    return 1;
}
