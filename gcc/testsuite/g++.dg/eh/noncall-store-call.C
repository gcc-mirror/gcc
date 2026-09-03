// A nothrow call can still trap when the caller stores its result.  Return
// slot optimization can fall back to a temporary and a caller-side copy.

// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -fdump-tree-eh -fdump-tree-optimized-eh" }

struct one
{
  __UINTPTR_TYPE__ value;
};

struct large
{
  __UINTPTR_TYPE__ value[8];
  ~large ();
};

extern one make_one () throw ();
extern large make_large () throw ();

void
store_one (one &dest)
{
  try
    {
      dest = make_one ();
    }
  catch (...)
    {
      __builtin_trap ();
    }
}

void *operator new (__SIZE_TYPE__, void *) throw ();

void
construct_large (large &dest)
{
  try
    {
      ::new (&dest) large (make_large ());
    }
  catch (...)
    {
      __builtin_trap ();
    }
}

// { dg-final { scan-tree-dump-times "__cxa_begin_catch" 2 "optimized" } }
