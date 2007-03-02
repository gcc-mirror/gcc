/* Verify that unwinding can find SPE registers in signal frames.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run { target { powerpc*-*-linux* && powerpc_spe } } } */
/* { dg-options "-fexceptions -fnon-call-exceptions -O2" } */

#include <unwind.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

int count;
char *null;
int found_reg;

typedef int v2si __attribute__((__vector_size__(8)));

v2si v1 = { 123, 234 };
v2si v2 = { 345, 456 };

static _Unwind_Reason_Code
force_unwind_stop (int version, _Unwind_Action actions,
                   _Unwind_Exception_Class exc_class,
                   struct _Unwind_Exception *exc_obj,
                   struct _Unwind_Context *context,
                   void *stop_parameter)
{
  unsigned int reg;
  if (actions & _UA_END_OF_STACK)
    abort ();
  if (_Unwind_GetGR (context, 1215) == 123)
    found_reg = 1;
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

static void counter (void *p __attribute__((unused)))
{
  ++count;
}

static void handler (void *p __attribute__((unused)))
{
  if (count != 2)
    abort ();
  if (!found_reg)
    abort ();
  exit (0);
}

static int __attribute__((noinline)) fn5 ()
{
  char dummy __attribute__((cleanup (counter)));
  force_unwind ();
  return 0;
}

static void fn4 (int sig)
{
  char dummy __attribute__((cleanup (counter)));
  /* Clobber high part without compiler's knowledge so the only saved
     copy is from the signal frame.  */
  asm volatile ("evmergelo 15,15,15");
  fn5 ();
  null = NULL;
}

static void fn3 ()
{
  abort ();
}

static int __attribute__((noinline)) fn2 ()
{
  register v2si r15 asm("r15");
  r15 = v1;
  asm volatile ("" : "+r" (r15));
  *null = 0;
  fn3 ();
  return 0;
}

static int __attribute__((noinline)) fn1 ()
{
  signal (SIGSEGV, fn4);
  signal (SIGBUS, fn4);
  fn2 ();
  return 0;
}

static int __attribute__((noinline)) fn0 ()
{
  char dummy __attribute__((cleanup (handler)));
  fn1 ();
  null = 0;
  return 0;
}

int main()
{ 
  fn0 ();
  abort ();
}
