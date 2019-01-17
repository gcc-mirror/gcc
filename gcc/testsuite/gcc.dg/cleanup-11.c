/* { dg-do run { target hppa*-*-hpux* *-*-linux* *-*-gnu* powerpc*-*-darwin* *-*-darwin[912]* } } */
/* { dg-options "-fexceptions -fnon-call-exceptions -O2" } */
/* { dg-require-effective-target exceptions } */
/* Verify that cleanups work with exception handling through realtime signal
   frames on alternate stack.  */

#include <unwind.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
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

int count;
char *null;

static void counter (void *p __attribute__((unused)))
{
  ++count;
}

static void handler (void *p __attribute__((unused)))
{
  if (count != 2)
    abort ();
  exit (0);
}

static int __attribute__((noinline)) fn5 ()
{
  char dummy __attribute__((cleanup (counter)));
  force_unwind ();
  return 0;
}

static void fn4 (int sig, siginfo_t *info, void *ctx)
{
  char dummy __attribute__((cleanup (counter)));
  fn5 ();
  null = NULL;
}

static void fn3 ()
{
  abort ();
}

static int __attribute__((noinline)) fn2 ()
{
  *null = 0;
  fn3 ();
  return 0;
}

static int __attribute__((noinline)) fn1 ()
{
  stack_t ss;
  struct sigaction s;

  ss.ss_size = 4 * sysconf (_SC_PAGESIZE);
  if (ss.ss_size < SIGSTKSZ)
    ss.ss_size = SIGSTKSZ;
  ss.ss_sp = malloc (ss.ss_size);
  if (ss.ss_sp == NULL)
    exit (1);
  ss.ss_flags = 0;
  if (sigaltstack (&ss, NULL) < 0)
    exit (1);

  sigemptyset (&s.sa_mask);
  s.sa_sigaction = fn4;
  s.sa_flags = SA_RESETHAND | SA_ONSTACK | SA_SIGINFO;
  sigaction (SIGSEGV, &s, NULL);
  sigaction (SIGBUS, &s, NULL);
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
