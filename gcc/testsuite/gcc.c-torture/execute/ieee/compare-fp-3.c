/* Copyright (C) 2004 Free Software Foundation.

   Test for composite comparison always true/false optimization.

   Written by Paolo Bonzini, 26th May 2004.  */

extern void link_error0 ();
extern void link_error1 ();

void
test1 (float x, float y)
{
  if ((x==y) && (x!=y))
    link_error0();
}

void
test2 (float x, float y)
{
  if ((x<y) && (x>y))
    link_error0();
}

void
test3 (float x, float y)
{
  if ((x<y) && (y<x))
    link_error0();
}

void 
test4 (float x, float y)
{
  if ((x==y) || (x!=y))
    {
    }
  else
    link_error1 ();
}

void
test5 (float x, float y)
{
  if (__builtin_isunordered (x, y) || (x>=y) || (x<y))
    {
    }
  else
    link_error1 ();
}

void
test6 (float x, float y)
{
  if (__builtin_isunordered (y, x) || (x<=y) || (y<x))
    {
    }
  else
    link_error1 ();
}

void
test7 (float x, float y)
{
  if (__builtin_isunordered (x, y) || !__builtin_isunordered (x, y))
    {
    }
  else
    link_error1 ();
}

void
all_tests (float x, float y)
{
  test1 (x, y);
  test2 (x, y);
  test3 (x, y);
  test4 (x, y);
  test5 (x, y);
  test6 (x, y);
  test7 (x, y);
}

int
main ()
{
  all_tests (0, 0);
  all_tests (1, 2);
  all_tests (4, 3);

  return 0;
}

#ifndef __OPTIMIZE__
void link_error0() {}
void link_error1() {}
#endif /* ! __OPTIMIZE__ */

