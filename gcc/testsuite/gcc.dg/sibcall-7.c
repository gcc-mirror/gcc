/* Simple check that tail recursive call optimization is also
   controlled by -foptimize-sibling-calls.

   Copyright (C) 2006 Free Software Foundation Inc.
   Original test by Hans-Peter Nilsson  <hp@bitrange.com>  */

/* On IA64 the call frame is allocated on the register stack, not the
   normal stack.  */

/* { dg-do run { target { ! "ia64-*-*" } } } */
/* { dg-options "-O2 -fno-optimize-sibling-calls" } */


extern void abort (void);

extern void recurser_void (int);
extern void track (int);

int main (void)
{
  recurser_void (0);
  return 0;
}

void recurser_void (int n)
{
  if (n == 0 || n == 7)
    track (n);

  if (n == 10)
    return;

  recurser_void (n + 1);
}

void *trackpoint;

void track (int n)
{
  char stackpos[1];

  if (n == 0)
    trackpoint = stackpos;
  else if (n != 7 || trackpoint == stackpos)
    abort ();
}
