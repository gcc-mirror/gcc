/* PR optimization/7520 */
/* ICE at -O3 on x86 due to register life problems caused by
   the return-without-value in bar.  */

int
foo ()
{
  int i;
  long long int j;

  while (1)
    {
      if (j & 1)
	++i;
      j >>= 1;
      if (j)
	return i;
    }
}

int
bar ()
{
  if (foo ())
    return;
}
