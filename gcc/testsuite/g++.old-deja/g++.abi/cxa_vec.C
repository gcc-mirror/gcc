// Test __cxa_vec routines
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Apr 2000 <nathan@nathan@codesourcery.com>

#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100
#include <cxxabi.h>
#include <stdio.h>
#include <new>
#include <stdlib.h>
#include <setjmp.h>

static int ctor_count = 0;
static int dtor_count = 0;
static bool dtor_repeat = false;

// our pseudo ctors and dtors
static void ctor (void *)
{
  if (!ctor_count)
    throw 1;
  ctor_count--;
}

static void dtor (void *)
{
  if (!dtor_count)
    {
      if (!dtor_repeat)
        dtor_count--;
      throw 1;
    }
  dtor_count--;
}

// track new and delete
static int blocks = 0;
void *operator new[] (std::size_t size) throw (std::bad_alloc)
{
  void *ptr = malloc (size);
  
  if (!ptr)
    throw std::bad_alloc ();
  blocks++;
  return ptr;
}

void operator delete[] (void *ptr) throw ()
{
  if (ptr)
    {
      free (ptr);
      blocks--;
    }
}
static jmp_buf jump;

// allocate and delete an array with no problems
void test0 ()
{
  static bool started = false;
  
  if (!started)
    {
      started = true;
      std::set_terminate (test0);
      
      ctor_count = dtor_count = 5;
      dtor_repeat = false;
      blocks = 0;
      
      try
        {
          void *ary = abi::__cxa_vec_new (5, 1, sizeof (std::size_t), ctor, dtor);
          abi::__cxa_vec_delete (ary, 1, sizeof (std::size_t), dtor);
          if (ctor_count || dtor_count || blocks)
            longjmp (jump, 1);
        }
      catch (...)
        {
          longjmp (jump, 2);
        }
    }
  else
    {
      longjmp (jump, 3);
    }
  return;
}

// allocate and delete an array with exception on ctor
void test1 ()
{
  static bool started = false;
  
  if (!started)
    {
      started = true;
      std::set_terminate (test1);
      
      ctor_count = dtor_count = 5;
      dtor_repeat = false;
      blocks = 0;
      
      ctor_count = 4;
      try
        {
          void *ary = abi::__cxa_vec_new (5, 1, sizeof (std::size_t), ctor, dtor);
          longjmp (jump, 1);
        }
      catch (...)
        {
          // we expect to get here
          if (ctor_count || dtor_count != 1 || blocks)
            longjmp (jump, 2);
        }
    }
  else
    {
      longjmp (jump, 3);
    }
  return;
}

// allocate and delete an array with exception on dtor
void test2 ()
{
  static bool started = false;
  
  if (!started)
    {
      started = true;
      std::set_terminate (test2);
      ctor_count = dtor_count = 5;
      dtor_repeat = false;
      blocks = 0;
      
      dtor_count = 3;
      try
        {
          void *ary = abi::__cxa_vec_new (5, 1, sizeof (std::size_t), ctor, dtor);
          abi::__cxa_vec_delete (ary, 1, sizeof (std::size_t), dtor);
          longjmp (jump, 1);
        }
      catch (...)
        {
          // we expect to get here
          if (ctor_count || dtor_count != -2u || blocks)
            longjmp (jump, 2);
        }
    }
  else
    {
      longjmp (jump, 3);
    }
  return;
}

// allocate an array with double exception on dtor
void test3 ()
{
  static bool started = false;
  
  if (!started)
    {
      started = true;
      std::set_terminate (test3);
      
      ctor_count = dtor_count = 5;
      dtor_repeat = false;
      blocks = 0;
  
      dtor_count = 3;
      dtor_repeat = true;
      try
        {
          void *ary = abi::__cxa_vec_new (5, 1, sizeof (std::size_t), ctor, dtor);
          abi::__cxa_vec_delete (ary, 1, sizeof (std::size_t), dtor);
          longjmp (jump, 1);
        }
      catch (...)
        {
          // we do not expect to get here
          longjmp (jump, 2);
        }
    }
  else
    {
      // we expect to get here (via terminate)
      if (ctor_count || dtor_count || blocks != 1)
	longjmp (jump, 3);
      longjmp (jump, -1);
    }
  return;
}

// allocate an array with exception on ctor and exception in cleanup
void test4 ()
{
  static bool started = false;
  
  if (!started)
    {
      started = true;
      std::set_terminate (test4);
      
      ctor_count = dtor_count = 5;
      dtor_repeat = false;
      blocks = 0;
  
      ctor_count = 3;
      dtor_count = 2;
      try
        {
          void *ary = abi::__cxa_vec_new (5, 1, sizeof (std::size_t), ctor, dtor);
          longjmp (jump, 1);
        }
      catch (...)
        {
          // we do not expect to get here
          longjmp (jump, 2);
        }
    }
  else
    {
      // we expect to get here (via terminate)
      if (ctor_count || dtor_count != -1u || blocks != 1)
        longjmp (jump, 3);
      longjmp (jump, -1);
    }
  return;
}

static void (*tests[])() =
{
  test0,
  test1,
  test2,
  test3,
  test4,
  NULL
};

int main ()
{
  int ix;
  int n;
  int errors = 0;
  
  for (ix = 0; tests[ix]; ix++)
    {
      if (n = setjmp (jump))
        {
          if (n > 0)
            {
              printf ("test %d failed %d\n", ix, n);
              errors++;
            }
        }
      else
        tests[ix] ();
    }
  return errors;
}

#else
int main ()
{
  return 0;
}
#endif
