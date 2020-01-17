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

void test (struct foo f)
{
  do_stuff ();

  calls_free (f.m_p);

  do_stuff ();

  calls_free (f.m_p); /* { dg-message "passing freed pointer '<unknown>' in call to 'calls_free' from 'test'" } */
  // TODO: something better than '<unknown>'

  do_stuff ();
}
