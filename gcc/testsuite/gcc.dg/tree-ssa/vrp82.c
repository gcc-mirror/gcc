/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error (void);

void
f2 (int c, int b)
{
  int s = 0;
  if (c == 0)
    s += 1;
  else if (c < 1)
    s -= 1;
  /* s in [-1, 1].   */
  b = (b & 1) + 1;
  /* b in range [1, 2].  */
  b = s << b;
  /* b in range [-4, 4].  */
  if (b == -5 || b == 5)
    link_error ();
}

void
f3 (int s, int b)
{
  if (s >> 3 == -2)
    {
      /* s in range [-16, -9].  */
      b = (b & 1) + 1;
      /* b in range [1, 2].  */
      b = s << b;
      /* b in range [bmin << smax, bmax << smin],
                    == [-16 << 2, -9 << 1]
                    == [-64, -18].  */
      if (b == -65 || b == -17)
	link_error ();
    }
}

void
f4 (unsigned int s, unsigned int b)
{
  s |= ~0xffU;
  /* s in [0xffffff00, 0xffffffff].  */
  b = (b & 1) + 1;
  /* b in [1, 2].  */
  b = s << b;
  /* s in [0xfffffc00, 0xfffffffe].  */
  if (b == ~0x3ffU - 1 || b == ~0x1U + 1)
    link_error ();
}

int
main ()
{
  return 0;
}
