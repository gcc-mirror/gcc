p1 (int b, int *p, int a)
{
  p[0] = p[1];
  return p[0];
}
p2 (int b, int *p, int a)
{
  p[0] = p[1];
  return p[0] == 0;
}
p3 (int b, int *p, int a)
{
  p[0] = p[1];
  a = p[0];
  if (a)
    return 0;
  return a;
}
p4 (int b, int *p, int a)
{
  a = p[1];
  p[0] = p[1];
  if (a)
    return 0;
  return a;
}
