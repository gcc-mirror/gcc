/* Simple check that sibling calls are performed from a
   void non-leaf-function taking one int argument calling itself.

   Copyright (C) 2002 Free Software Foundation Inc.
   Contributed by Hans-Peter Nilsson  <hp@bitrange.com>  */

/* { dg-do run } */
/* { dg-options "-O2 -foptimize-sibling-calls" } */

/* The option -foptimize-sibling-calls is the default, but serves as
   marker.  Self-recursion tail calls are optimized for all targets,
   regardless of presence of sibcall patterns.  */

extern void abort (void);
extern void exit (int);

extern void recurser_void (int);
extern void track (int);

int main ()
{
  recurser_void (0);
  exit (0);
}

void
recurser_void (int n)
{
  if (n == 0 || n == 7)
    track (n);

  if (n == 10)
    return;

  recurser_void (n + 1);
}

void *trackpoint;

void
track (int n)
{
  char stackpos[1];

  if (n == 0)
    trackpoint = stackpos;
  else if (n != 7 || trackpoint != stackpos)
    abort ();
}
