/* PR optimization/10024 */
extern int *allegro_errno;
typedef long fixed;
extern inline int
fixfloor (fixed x)
{
  if (x >= 0)
    return (x >> 16);
  else
    return ~((~x) >> 16);
}
extern inline int
fixtoi (fixed x)
{
  return fixfloor (x) + ((x & 0x8000) >> 15);
}
extern inline fixed
ftofix (double x)
{
  if (x > 32767.0)
    {
      *allegro_errno = 34;
      return 0x7FFFFFFF;
    }
  if (x < -32767.0)
    {
      *allegro_errno = 34;
      return -0x7FFFFFFF;
    }
  return (long) (x * 65536.0 + (x < 0 ? -0.5 : 0.5));
}
extern inline double
fixtof (fixed x)
{
  return (double) x / 65536.0;
}
extern inline fixed
fixdiv (fixed x, fixed y)
{
  if (y == 0)
    {
      *allegro_errno = 34;
      return (x < 0) ? -0x7FFFFFFF : 0x7FFFFFFF;
    }
  else
    return ftofix (fixtof (x) / fixtof (y));
}
extern inline fixed
itofix (int x)
{
  return x << 16;
}

int
foo (int n)
{
  return fixtoi (fixdiv (itofix (512), itofix (n)));
}
