long long
simple_rand ()
{
  static unsigned long long seed = 47114711;
  unsigned long long this = seed * 1103515245 + 12345;
  seed = this;
  return this >> 8;
}

unsigned long long int
random_bitstring ()
{
  unsigned long long int x;
  int n_bits;
  long long ran;
  int tot_bits = 0;

  x = 0;
  for (;;)
    {
      ran = simple_rand ();
      n_bits = (ran >> 1) % 16;
      tot_bits += n_bits;

      if (n_bits == 0)
	return x;
      else
	{
	  x <<= n_bits;
	  if (ran & 1)
	    x |= (1 << n_bits) - 1;

	  if (tot_bits > 8 * sizeof (long long) + 6)
	    return x;
	}
    }
}

#define ABS(x) ((x) >= 0 ? (x) : -(x))

main ()
{
  long long int i;

  for (i = 0; i < 10000; i++)
    {
      unsigned long long x, y;
      x = random_bitstring ();
      y = random_bitstring ();

      if (sizeof (int) == sizeof (long long))
	goto save_time;

      { unsigned long long xx = x, yy = y, r1, r2;
	if (yy == 0) continue;
	r1 = xx / yy;
	r2 = xx % yy;
	if (r2 >= yy || r1 * yy + r2 != xx)
	  abort ();
      }
      { signed long long xx = x, yy = y, r1, r2;
	if ((unsigned long long) xx << 1 == 0 && yy == -1)
	  continue;
	r1 = xx / yy;
	r2 = xx % yy;
	if (ABS (r2) >= (unsigned long long) ABS (yy) || (signed long long) (r1 * yy + r2) != xx)
	  abort ();
      }
    save_time:
      { unsigned int xx = x, yy = y, r1, r2;
	if (yy == 0) continue;
	r1 = xx / yy;
	r2 = xx % yy;
	if (r2 >= yy || r1 * yy + r2 != xx)
	  abort ();
      }
      { signed int xx = x, yy = y, r1, r2;
	if ((unsigned int) xx << 1 == 0 && yy == -1)
	  continue;
	r1 = xx / yy;
	r2 = xx % yy;
	if (ABS (r2) >= (unsigned int) ABS (yy) || (signed int) (r1 * yy + r2) != xx)
	  abort ();
      }
      { unsigned short xx = x, yy = y, r1, r2;
	if (yy == 0) continue;
	r1 = xx / yy;
	r2 = xx % yy;
	if (r2 >= yy || r1 * yy + r2 != xx)
	  abort ();
      }
      { signed short xx = x, yy = y, r1, r2;
	r1 = xx / yy;
	r2 = xx % yy;
	if (ABS (r2) >= (unsigned short) ABS (yy) || (signed short) (r1 * yy + r2) != xx)
	  abort ();
      }
      { unsigned char xx = x, yy = y, r1, r2;
	if (yy == 0) continue;
	r1 = xx / yy;
	r2 = xx % yy;
	if (r2 >= yy || r1 * yy + r2 != xx)
	  abort ();
      }
      { signed char xx = x, yy = y, r1, r2;
	r1 = xx / yy;
	r2 = xx % yy;
	if (ABS (r2) >= (unsigned char) ABS (yy) || (signed char) (r1 * yy + r2) != xx)
	  abort ();
      }
    }

  exit (0);
}
