
cc1 (x, y)
     int x, y;
{
  int z;
  z = x - y;
  if (x >= y)
    return z + 1;
  else
    return z + 0;
}

cc2 (x, y)
     int x, y;
{
  int z;

  z = x - y;
  if (z >= 0)
    return z + 1;
  else
    return z + 0;
}

cc3 (x, y)
     unsigned x, y;
{
  unsigned z;
  z = x - y;
  if (x >= y)
    return z + 1;
  else
    return z + 0;
}

cc4 (x, y)
     unsigned x, y;
{
  unsigned z;

  z = x - y;
  if (z >= 0)
    return z + 1;
  else
    return z + 0;
}
