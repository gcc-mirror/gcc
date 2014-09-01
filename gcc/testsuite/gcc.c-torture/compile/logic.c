logic (a, b)
     int *a, b;
{
  if (*a & 123)
    b = 1;
  if (*a & ~222)
    b = 2;
  if (124 & *a)
    b = 3;
  if (~111 & *a)
    b = 4;

  if (~*a & 23)
    b = 1;
  if (~*a & ~22)
    b = 2;
  if (24 & ~*a)
    b = 3;
  if (~11 & ~*a)
    b = 4;

  if (~*a & b)
    b = 1;
  if (~*a & ~b)
    b = 2;
  if (*a & ~*a)
    b = 3;
  return b;
}

x (a, b, c)
{
  for (a = 0;  --a > 0;);
  for (b = -1; --b > 0;);
  for (c = -65536; --c > 0;);
  return a + b + c;
}
