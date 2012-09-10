/* { dg-do link } */
/* { dg-options "-O2 -fno-tree-switch-conversion" } */

extern void link_error (void);

/* Test <<.  */

void
f3 (int s, int b)
{
  if (s >> 3 == -2)
    /* s in range [-16, -9].  */
    {
      s += 17;
      /* s in range [1, 8].  */
      b = (b & 1) + 1;
      /* b in range [1, 2].  */
      b =  b << s;
      /* b in range [bmin << smin, bmax << smax],
                    == [1 << 1, 2 << 8]
                    == [2, 512].  */
      if (b == 1 || b == 513)
	link_error ();
    }
}

int
main ()
{
  return 0;
}


