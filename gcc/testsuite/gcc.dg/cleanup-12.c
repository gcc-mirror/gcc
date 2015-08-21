/* PR middle-end/32758 */
/* HP-UX libunwind.so doesn't provide _UA_END_OF_STACK */
/* { dg-do run } */
/* { dg-options "-O2 -fexceptions" } */
/* { dg-skip-if "" { "ia64-*-hpux11.*" }  { "*" } { "" } } */
/* { dg-skip-if "" { ! nonlocal_goto } { "*" } { "" } } */
/* Verify unwind info in presence of alloca.  */

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

static void force_unwind (void)
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

__attribute__((noinline))
void foo (void *x __attribute__((unused)))
{
  force_unwind ();
}

__attribute__((noinline))
int bar (unsigned int x)
{
  void *y = __builtin_alloca (x);
  foo (y);
  return 1;
}

static void handler (void *p __attribute__((unused)))
{
  exit (0);
}

__attribute__((noinline))
static void doit ()
{
  char dummy __attribute__((cleanup (handler)));
  bar (1024);
}

int main ()
{
  doit ();
  abort ();
}
