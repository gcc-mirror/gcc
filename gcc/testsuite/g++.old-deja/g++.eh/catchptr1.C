// { dg-do run  }
// Test pointer chain catching
// Copyright (C) 2000, 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 9 Apr 2000 <nathan@nathan@codesourcery.com>

#include <stdio.h>

void fn () {}
struct A {void fn () {}};
static int var = 1;
static const int const_var = 2;

struct B;
struct C;

int test0 ()
{
  try
    {
      throw &fn;
    }
  catch (void *)
    {
      // should not decay to void *
      return 1;
    }
  catch (...)
    {
      return 0;
    }
  return -1;
}

int test1 ()
{
  try
    {
      throw &A::fn;
    }
  catch (void *)
    {
      // should not decay to void *
      return 1;
    }
  catch (...)
    {
      return 0;
    }
  return -1;
}

int test2 ()
{
  try
    {
      throw &var;
    }
  catch (void *)
    {
      // should decay to void *
      return 0;
    }
  catch (...)
    {
      return 1;
    }
  return -1;
}

int test3 ()
{
  try
    {
      throw &var;
    }
  catch (void const *)
    {
      // should decay to const void *
      return 0;
    }
  catch (...)
    {
      return 1;
    }
  return -1;
}

int test4 ()
{
  try
    {
      throw &const_var;
    }
  catch (void *)
    {
      // should not decay to void *
      return 1;
    }
  catch (void const *)
    {
      // should decay to const void *
      return 0;
    }
  catch (...)
    {
      return 2;
    }
  return -1;
}

int test5 ()
{
  try
    {
      throw (void ***)0;
    }
  catch (void ***)
    {
      return 0;
    }
  catch (...)
    {
      return 1;
    }
  return -1;
}

int test6 ()
{
  try
    {
      throw (void const* const* const*)0;
    }
  catch (void ***)
    {
      return 1;
    }
  catch (void * const* const*)
    {
      return 2;
    }
  catch (void const* * const*)
    {
      return 3;
    }
  catch (void const* const* *)
    {
      return 4;
    }
  catch (void const* const* const *)
    {
      return 0;
    }
  catch (...)
    {
      return 1;
    }
  return -1;
}

int test7 ()
{
  try
    {
      throw (void ***)0;
    }
  catch (void const* const**)
    {
      return 1;
    }
  catch (void const** const *)
    {
      return 2;
    }
  catch (void * const* const *)
    {
      return 0;
    }
  catch (...)
    {
      return 3;
    }
  return -1;
}

int test8 ()
{
  try
    {
      throw (B **)0;
    }
  catch (C **)
    {
      return 1;
    }
  catch (B **)
    {
      return 0;
    }
  catch (...)
    {
      return 2;
    }
  return -1;
}

int test9 ()
{
  try
    {
      throw (B **)0;
    }
  catch (C const *const *)
    {
      return 1;
    }
  catch (B const *const *)
    {
      return 0;
    }
  catch (...)
    {
      return 2;
    }
  return -1;
}

static int (*tests[])() =
{
  test0,
  test1,
  test2,
  test3,
  test4,
  
  test5,
  test6,
  test7,
  
  test8,
  test9,
  
  NULL
};

int main ()
{
  int ix;
  int errors = 0;
  
  for (ix = 0; tests[ix]; ix++)
    {
      int n = tests[ix] ();
      
      if (n)
        {
          printf ("test %d failed %d\n", ix, n);
          errors++;
        }
    }
  return errors;
}
