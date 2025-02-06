/* HP-UX libunwind.so doesn't provide _UA_END_OF_STACK */
/* { dg-do run } */
/* { dg-options "-fexceptions" } */
/* { dg-skip-if "" { "ia64-*-hpux11.*" } } */
/* { dg-require-effective-target exceptions } */
/* Verify that cleanups work with exception handling.  */

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

static void force_unwind ()
{
  struct _Unwind_Exception *exc = malloc (sizeof (*exc));
  memset (&exc->exception_class, 0, sizeof (exc->exception_class));
  exc->exception_cleanup = 0;
                   
#ifndef __USING_SJLJ_EXCEPTIONS__
  _Unwind_ForcedUnwind (exc, force_unwind_stop, 0);
#else
  _Unwind_SjLj_ForcedUnwind (exc, force_unwind_stop, 0);
#endif
                   
  abort ();
}

static void handler (void *p __attribute__((unused)))
{
  exit (0);
}

static void doit ()
{
  char dummy __attribute__((cleanup (handler)));
  force_unwind ();
}

int main()
{ 
  doit ();
  abort ();
}
