// HP-UX libunwind.so doesn't provide _UA_END_OF_STACK.
// { dg-do run { xfail "ia64-hp-hpux11.*" } }

// Test that leaving the catch block without rethrowing
// does call the exception object destructor.

#include <unwind.h>
#include <stdlib.h>

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

static void
force_unwind_cleanup (_Unwind_Reason_Code, struct _Unwind_Exception *)
{
  exit (0);
}

static void
force_unwind ()
{
  _Unwind_Exception *exc = new _Unwind_Exception;
  exc->exception_class = 0;
  exc->exception_cleanup = force_unwind_cleanup;

#ifndef __USING_SJLJ_EXCEPTIONS__
  _Unwind_ForcedUnwind (exc, force_unwind_stop, 0);
#else
  _Unwind_SjLj_ForcedUnwind (exc, force_unwind_stop, 0);
#endif

  abort ();
}

int main()
{ 
  try {
    force_unwind ();
  } catch (...) {
  }
  abort ();
}
