// Build don't link:

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Apr 1999 <nathan@acm.org>

// delete (void *)e and delete[] (void *)e result in undefined behaviour
// [expr.delete/3]. Check we warn about them
// operator new functions can only return NULL, if their exceptions
// specification is `throw()'. All other cases must return a non-null pointer
// [expr.new/13].

void *operator new(unsigned)
{
  return 0; // ERROR - cannot return NULL
}
void *operator new[](unsigned)
{
  return 0; // ERROR - cannot return NULL
}

struct X
{
  void *operator new(unsigned)
  {
    return 0; // ERROR - cannot return NULL
  }
  void *operator new[](unsigned)
  {
    return 0; // ERROR - cannot return NULL
  }
};

struct Y
{
  void *operator new(unsigned) throw()
  {
    return 0; // ok
  }
  void *operator new[](unsigned) throw()
  {
    return 0; // ok
  }
};

void fn(double *d, void *v)
{
  delete d;   // ok
  delete v;   // WARNING - deleting void
  delete[] d; // ok
  delete[] v; // WARNING - deleting void
}
