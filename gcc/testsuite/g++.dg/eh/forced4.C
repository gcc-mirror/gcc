// HP-UX libunwind.so doesn't provide _UA_END_OF_STACK.
// { dg-do run { xfail "ia64-hp-hpux11.*" } }

// Test that forced unwinding does not call std::unexpected going 
// throw a function with a non-empty exception spec.

#include <unwind.h>
#include <stdlib.h>
#include <string.h>

static _Unwind_Reason_Code
force_unwind_stop (int version, _Unwind_Action actions,
                   _Unwind_Exception_Class exc_class,
                   struct _Unwind_Exception *exc_obj,
                   struct _Unwind_Context *context,
                   void *stop_parameter)
{
  if (actions & _UA_END_OF_STACK)
    abort ();
  return _URC_NO_REASON;
}

static void __attribute__((noreturn))
force_unwind ()
{
  _Unwind_Exception *exc = new _Unwind_Exception;
  // exception_class might not be a scalar.
  memset (&exc->exception_class, 0, sizeof (exc->exception_class));
  exc->exception_cleanup = 0;

#ifndef __USING_SJLJ_EXCEPTIONS__
  _Unwind_ForcedUnwind (exc, force_unwind_stop, 0);
#else
  _Unwind_SjLj_ForcedUnwind (exc, force_unwind_stop, 0);
#endif

  abort ();
}

static void
doit ()
#if __cplusplus <= 201402L
throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif
{
  force_unwind ();
}

int main()
{ 
  try {
    doit ();
  } catch (...) {
  }
}
