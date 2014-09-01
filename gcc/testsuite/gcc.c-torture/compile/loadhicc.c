typedef int xtype;

foo (p, pc)
     xtype *p;
     char *pc;
{
  xtype a;
  unsigned b = 0;

  a = *p;
  p[1] = a;
  if ((unsigned) p[1] > 0)
    return 1;
  return a;
}
