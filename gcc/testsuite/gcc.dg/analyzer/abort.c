#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "analyzer-decls.h"

extern void foo ();
extern void bar ();

void test_1 (int i)
{
  if (i == 42)
    abort ();

  __analyzer_eval (i != 42); /* { dg-warning "TRUE" } */
}

void test_2 (int i)
{
  if (i)
    foo ();
  else
    bar ();

  foo ();

  if (i)
    foo ();
  else
    abort ();

  __analyzer_eval (i != 0); /* { dg-warning "TRUE" } */
}

/**************************************************************************/

void calls_abort (const char *msg)
{
  fprintf (stderr, "%s", msg);
  abort ();
}

void test_3 (void *ptr)
{
  if (!ptr)
    calls_abort ("ptr was NULL");

  __analyzer_eval (ptr != 0); /* { dg-warning "TRUE" } */
}

/**************************************************************************/

extern void marked_noreturn (const char *msg)
  __attribute__ ((__noreturn__));

void test_4 (void *ptr)
{
  if (!ptr)
    marked_noreturn ("ptr was NULL");

  __analyzer_eval (ptr != 0); /* { dg-warning "TRUE" } */
}

/**************************************************************************/

void test_5 (int i)
{
  assert (i < 10);

  /* We have not defined NDEBUG, so this will call __assert_fail if
     i >= 10, which is labelled with __attribute__ ((__noreturn__)).  */
  __analyzer_eval (i < 10); /* { dg-warning "TRUE" } */
}
