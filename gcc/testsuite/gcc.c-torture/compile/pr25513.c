int f(void)
{
  static _Complex double t;
  int i, j;
  for(i = 0;i<2;i++)
    for(j = 0;j<2;j++)
      t = .5 * 1.0;
  return t;
}
