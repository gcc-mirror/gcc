double
u2d (unsigned int u)
{
  return u;
}

double
i2d (signed int i)
{
  return i;
}

unsigned int
d2u (double d)
{
  return d;
}

signed int
d2i (double d)
{
  return d;
}

int
main (void)
{
  __builtin_printf ("%lf, %lf, %lf\n", u2d (~0), u2d (1 << 31), u2d (1));
  __builtin_printf ("%lf, %lf, %lf\n", i2d (~0), i2d (1 << 31), i2d (1));

  __builtin_printf ("%u, %u, %u\n",
		    d2u (u2d (~0)), d2u (u2d (1 << 31)), d2u (u2d (1)));
  __builtin_printf ("%d, %d, %d\n",
		    d2i (i2d (~0)), d2i (i2d (1 << 31)), d2i (i2d (1)));
}
