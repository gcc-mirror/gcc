/* { dg-do run { target *-*-linux* } } */
/* { dg-additional-sources "../sync-1.c" } */
/* { dg-options "-Dop -Dtype=short -mno-unaligned-atomic-may-use-library" } */

/* Make sure we get a SIGTRAP or equivalent when passing unaligned
   but otherwise valid pointers to the atomic builtins.  */

#include <signal.h>
#include <stdlib.h>

#ifndef type
#error type not defined
#endif

#ifndef op
#define op 0
#endif

#ifndef xchg
#define xchg 0
#endif

#if op
int sfa (type *p, type *q, int a);
#endif

#if xchg
void acen (type *ptr, type *val, type *ret);
#endif

#ifndef misalignment
#define misalignment 1
#endif

volatile int trap_expected = 0;

struct { char x[misalignment]; type i; } s __attribute__ ((__aligned__ (4)))
  = { {0}, (type) 0xdeadbeef };
type x = 2;
type ret = 42;

#ifdef TRAP_USING_ABORT
#define SYMSTR(x) STR1(__USER_LABEL_PREFIX__, x)
#define STR1(x,y) STR2(x, y)
#define STR2(x,y) #x #y
/* LTO requires marking seemingly-unused-but-used global functions.  */
void my_abort (void) __asm__ (SYMSTR (abort)) __attribute__ ((__used__));
void my_abort (void)
#else
#ifdef __gnu_linux__
void trap_handler(int signum)
#else
#error "can't catch break 8"
#endif
#endif
{
  if (1
#ifndef TRAP_USING_ABORT
      && signum == SIGTRAP
#endif
      && trap_expected
      && s.i == (type) 0xdeadbeef
      && x == 2 && ret == 42)
    exit (0);

#ifdef TRAP_USING_ABORT
  /* We might be able to trust the exit-value getting through, but add
     a NULL-dereference SEGV just in case.  Make sure gcc doesn't
     understand the NULL.  */
  *({ int *p; asm ("" : "=rm" (p) : "0" (0)); p; }) = 0xdead;
  exit (2);
#else
  abort ();
#endif
}

int main(void)
{
  type ret;

#ifndef TRAP_USING_ABORT
#ifdef __gnu_linux__
  if (signal (SIGTRAP, trap_handler) == SIG_ERR)
    abort ();
#endif
#endif

#ifndef mis_ok
  trap_expected = 1;
#endif

#if op
  sfa (&s.i, &s.i, 42);

  /* We should have fallen into the trap now.  But don't call abort
     yet: if the trap is implemented as a call to abort, we have to
     tell the difference.  Set a global variable *and* make sure the
     setting isn't eliminated by optimizers: another call to sfa
     should do it.  */
  trap_expected = 0;

#ifdef mis_ok
  /* We're missing a sequence point, but we shouldn't have the initial
     value.  */
  if (s.i == (type) 0xdeadbeef)
    abort ();
  exit (0);
#endif

  sfa (&x, &x, 1);
#else
  acen (&s.i, &x, &ret);

#ifdef mis_ok
  if (s.i != 2 || x != 2 || ret != (type) 0xdeadbeef)
    abort ();
  exit (0);
#endif

  trap_expected = 0;

  acen (&x, &x, &ret);
#endif

  abort ();
}
