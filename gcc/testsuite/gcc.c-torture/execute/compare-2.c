/* Copyright (C) 2002 Free Software Foundation.

   Ensure that the composite comparison optimization doesn't misfire
   and attempt to combine a signed comparison with an unsigned one.

   Written by Roger Sayle, 3rd June 2002.  */

extern void abort (void);

int
foo (int x, int y)
{
  /* If miscompiled the following may become "x == y".  */
  return (x<=y) && ((unsigned int)x >= (unsigned int)y);
}

int
main ()
{
  if (! foo (-1,0))
    abort ();
  return 0;
}

