// { dg-do run }
// { dg-options "-fforced-unwind-exceptions" }

// Test that forced unwinding runs all cleanups, and only cleanups.

#include <unwind.h>
#include <stdlib.h>

static int test = 0;

static _Unwind_Reason_Code
force_unwind_stop (int version, _Unwind_Action actions,
                   _Unwind_Exception_Class exc_class,
                   struct _Unwind_Exception *exc_obj,
                   struct _Unwind_Context *context,
                   void *stop_parameter)
{
  if (actions & _UA_END_OF_STACK)
    {
      if (test != 5)
        abort ();
      exit (0);
    }

  return _URC_NO_REASON;
}

// Note that neither the noreturn nor the nothrow specification
// affects forced unwinding.

static void __attribute__((noreturn))
force_unwind () throw()
{
  _Unwind_Exception *exc = new _Unwind_Exception;
  exc->exception_class = 0;
  exc->exception_cleanup = 0;
                   
  _Unwind_ForcedUnwind (exc, force_unwind_stop, 0);
                   
  abort ();
}

struct S
{
  int bit;
  S(int b) : bit(b) { }
  ~S() { test |= bit; }
};
  
static void doit_3 ()
{
  S one(1);
  force_unwind ();
}

static void doit_2 ()
{
  try {
    doit_3 ();
  } catch (...) {
    test |= 2;
  }
}

static void doit_1 ()
{
  S four(4);
  doit_2 ();
}

static void doit ()
{
  try {
    doit_1 ();
  } catch(...) {
    test |= 8;
  }
}

int main()
{ 
  try {
    doit ();
  } catch (...) {
  }
  abort ();
}
