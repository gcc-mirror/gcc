/* { dg-additional-options "-fanalyzer-verbosity=1" } */

#include <stdlib.h>

void
calls_free (void *victim)
{
  free (victim); /* { dg-warning "double-'free' of 'victim'" } */
}

extern void do_stuff (void);

struct foo
{
  void *m_p;
};

static void *  __attribute__((noinline))
test_a (struct foo f)
{
  do_stuff ();

  calls_free (f.m_p);

  do_stuff ();

  return f.m_p;
}

void test_b (void *p)
{
  void *q;
  struct foo f;
  f.m_p = p;
  q = test_a (f);
  calls_free (q); /* { dg-message "passing freed pointer 'q' in call to 'calls_free' from 'test_b'" } */
  do_stuff ();
}
