#include <stdlib.h>

int foo ();
int bar ();

/* Verify that only significant edges are reported.  */

void test (int a, int b, int c)
{
  void *p = malloc (1024); /* { dg-message "allocated here" } */
  while (a) /* { dg-bogus "" } */
    foo ();
  if (b) /* { dg-bogus "" } */
    foo ();
  else
    bar ();
  if (c) /* { dg-message "following 'true' branch" } */
    free (p); /* { dg-message "first 'free' here" } */
  free (p); /* { dg-warning "double-'free' of 'p'" } */
}
