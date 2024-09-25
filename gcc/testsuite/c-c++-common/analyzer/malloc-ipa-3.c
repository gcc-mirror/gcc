/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

void *
calls_malloc (void)
{
  void *result = malloc (1024);
  return result;
}

void
calls_free (void *victim)
{
  free (victim); /* { dg-warning "double-'free' of 'victim'" } */
  /* TODO: this would be better emitted at the callsite,
     for such a simple wrapper.  */
}

void test (void)
{
  void *ptr = calls_malloc ();
  calls_free (ptr);
  calls_free (ptr); /* BUG: double-'free'.  */
}
