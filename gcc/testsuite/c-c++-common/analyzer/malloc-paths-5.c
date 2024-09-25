/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdio.h>
#include <stdlib.h>

extern void do_stuff (void);

int test (const char *filename, int flag)
{
  FILE *f;
  int *p, *q;
  int i;

  p = (int *)malloc (sizeof (int)); /* { dg-line malloc_of_p } */
  if (!p) /* { dg-line test_of_p } */
    {
      free (p);
      return -1;
    }

  q = (int *)malloc (sizeof (int)); /* { dg-line malloc_of_q } */
  if (!q) /* { dg-line test_of_q } */
    {
      free (p); /* { dg-line first_free_of_p } */
      /* oops: forgot the "return" here, so it falls through.  */
    }

  do_stuff ();

  free (p); /* { dg-line second_free_of_p } */
  free (q);
  return 0;

  /* { dg-warning "double-'free' of 'p'" "warning" { target *-*-* } second_free_of_p } */
  /* { dg-message "\\(1\\) allocated here" "event 1" { target *-*-* } malloc_of_p } */
  /* { dg-message "\\(2\\) assuming 'p' is non-NULL" "event 2" { target *-*-* } test_of_p } */
  /* { dg-message "\\(3\\) following 'false' branch \\(when 'p' is non-NULL\\)\\.\\.\\." "event 3" { target *-*-* } test_of_p } */
  /* { dg-message "\\(4\\) \\.\\.\\.to here" "event 4" { target *-*-* } malloc_of_q } */
  /* { dg-message "\\(5\\) following 'true' branch \\(when 'q' is NULL\\)\\.\\.\\." "event 5" { target *-*-* } test_of_q } */
  /* { dg-message "\\(6\\) \\.\\.\\.to here" "event 6" { target *-*-* } first_free_of_p } */
  /* { dg-message "\\(7\\) first 'free' here" "event 7" { target *-*-* } first_free_of_p } */
  /* { dg-message "\\(8\\) second 'free' here; first 'free' was at \\(7\\)" "event 8" { target *-*-* } second_free_of_p } */

  /* We don't care about the state changes to q.  */
}
