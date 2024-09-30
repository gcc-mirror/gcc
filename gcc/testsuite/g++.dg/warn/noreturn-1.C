// Test that noreturn attributes are properly set.
// Origin: Kaveh Ghazi <ghazi@caip.rutgers.edu> 2002-06-18.
// { dg-do compile }
// { dg-options "-Wall -O2" }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

#include <cstdlib>

int foo1 (int i)
{
  switch (i)
    {
    case 1:
    case 2:
      return i;
    }
  abort();
}

int foo2 (int i)
{
  switch (i)
    {
    case 1:
    case 2:
      return i;
    }
  std::abort();
}

int foo3 (int i)
{
  switch (i)
    {
    case 1:
    case 2:
      return i;
    }
  exit(1);
}

int foo4 (int i)
{
  switch (i)
    {
    case 1:
    case 2:
      return i;
    }
  std::exit(1);
}

void __attribute__ ((__noreturn__)) foo5 ()
{
  abort();
}

void __attribute__ ((__noreturn__)) foo6 ()
{
  std::abort();
}

void __attribute__ ((__noreturn__)) foo7 ()
{
  exit(1);
}

void __attribute__ ((__noreturn__)) foo8 ()
{
  std::exit(1);
}

