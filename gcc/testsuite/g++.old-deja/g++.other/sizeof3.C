// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Sep 1999 <nathan@acm.org>

// C++ does not decay lvalues into rvalues until as late as possible. This
// means things like the rhs of a comma operator mustn't decay. This will make
// a difference if it is an array or function.

extern void abort();

int main (int argc, char **argv)
{
  int ary[10];
  int ary1[10];
  
  if (sizeof (0,ary) != sizeof (ary))
    abort ();
  if (sizeof (argc ? ary : ary1) != sizeof (ary))
    abort ();
  return 0;
}
