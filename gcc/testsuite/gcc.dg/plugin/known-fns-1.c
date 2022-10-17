/* { dg-do compile } */
/* { dg-options "-fanalyzer" } */
/* { dg-require-effective-target analyzer } */

#include "../analyzer/analyzer-decls.h"

/* Basic example of known fn behavior.  */

extern int returns_42 (void);

void test_1 (void)
{
  int val = returns_42 ();
  __analyzer_eval (val == 42); /* { dg-warning "TRUE" } */
}

/* Example of bifurcation, with a copy that can fail.  */

extern int
attempt_to_copy (void *to, const void *from, int sz);

void test_copy_success (void *to, const void *from, int sz)
{
  if (!attempt_to_copy (to, from, sz))
    {
      /* Success */
    }
}

void test_copy_failure (void *to, const void *from, int sz)
{
  if (attempt_to_copy (to, from, sz)) /* { dg-message "when 'attempt_to_copy' fails" } */
    __analyzer_dump_path (); /* { dg-message "path" } */
}

struct coord
{
  int x;
  int y;
  int z;
};

void test_copy_2 (void)
{
  struct coord to = {1, 2, 3};
  struct coord from = {4, 5, 6};
  if (attempt_to_copy (&to, &from, sizeof (struct coord)))
    {
      /* Failure.  */
      __analyzer_eval (to.x == 1); /* { dg-warning "TRUE" } */
      __analyzer_eval (to.y == 2); /* { dg-warning "TRUE" } */
      __analyzer_eval (to.z == 3); /* { dg-warning "TRUE" } */
    }
  else
    {
      /* Success.  */
      __analyzer_eval (to.x == 4); /* { dg-warning "TRUE" } */
      __analyzer_eval (to.y == 5); /* { dg-warning "TRUE" } */
      __analyzer_eval (to.z == 6); /* { dg-warning "TRUE" } */
    }
}
