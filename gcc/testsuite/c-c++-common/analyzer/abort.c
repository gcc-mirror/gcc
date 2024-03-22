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

/* Verify that we discover conditions from assertions if the assert macro
   isn't disabled, and that it has its failure-handler labelled with
   __attribute__ ((__noreturn__)).
   This attribute isn't present for all implementations of <assert.h>, so
   we have to test the idea using our own assert macro.  */

extern void my_assert_fail (const char *expr, const char *file, int line)
  __attribute__ ((__noreturn__));

#define MY_ASSERT(EXPR) \
  do { if (!(EXPR)) my_assert_fail (#EXPR, __FILE__, __LINE__); } while (0)

void test_5 (int i)
{
  MY_ASSERT (i < 10);
  __analyzer_eval (i < 10); /* { dg-warning "TRUE" } */
}
