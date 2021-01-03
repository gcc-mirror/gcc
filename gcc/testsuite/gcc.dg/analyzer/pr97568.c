#include "analyzer-decls.h"

#define NULL ((void *)0)

extern int *const p1;

int *const p2;

int v3;
extern int *const p3 = &v3; /* { dg-warning "'p3' initialized and declared 'extern'" } */

int v4;
int *const p4 = &v4;

int main (void)
{
  __analyzer_describe (0, p1); /* { dg-message "INIT_VAL\\(p1\\)" } */
  __analyzer_eval (p1 == NULL); /* { dg-message "UNKNOWN" } */

  __analyzer_eval (p2 == NULL); /* { dg-message "TRUE" } */

  __analyzer_describe (0, p3); /* { dg-message "&v3" } */
  __analyzer_eval (p3 == NULL); /* { dg-message "FALSE" } */

  __analyzer_describe (0, p4); /* { dg-message "&v4" } */
  __analyzer_eval (p4 == NULL); /* { dg-message "FALSE" } */

  return p1[0];
}
