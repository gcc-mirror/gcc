cc8 (a, b)
{
  if (a < 0)
    goto L1;
    if (a == 0)
      goto L2;
 L1:b++;
 L2:b++;
  return b;
}

cc7 (a)
     long long a;
{
  if (a < 0)
    return 1;
  else
    return 0;
}

cc6 (float a, double p)
{
  p = a;
  if (p < 0)
    return p;
  else
    return p + 1;
}

cc5 (p, a)
     char *p;
     char a;
{
  p[2] = a;
  if (a)
    return 0;
  else
    return 1;
}


cc4 (a, b, p)
     int a, b;
     int *p;
{
  a = (int short)b;
  *p = a;
  if ((int) a < 0)
    return 0;
  else
    return 1;
}


cc1 (a, b)
{
  int x = 0;

  if ((int) a < (int) b)
    {
      if ((unsigned) a < (unsigned) b)
	x++;
      x++;
    }

  return x;
}

cc2 (a, b)
{
  int x = 0;

  if ((int) a <= (int) b)
    {
      if ((int) a < (int) b)
	x++;
      x++;
    }

  return x;
}

cc3 (a, b)
{
  int x = 0;

  a += b;
  if ((unsigned) a > 0)
    {
      if (a == 0)
	x++;
      x++;
    }

  return x;
}
