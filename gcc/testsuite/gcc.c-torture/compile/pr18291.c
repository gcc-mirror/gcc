int baz(int k2)
{
  int i, j, *p, k = 1, k1 = 0;
  if (k2)
    p = &j;
  else
    p = &i;
  if (k1)
    *p = 0 , p = &k;
  *p = 1;
  return k;
}
