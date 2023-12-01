/* Example of a fix-it hint that adds a #include directive,
   adding them after a pre-existing #include directive.  */
#include "empty.h"
int the_next_line;

/* { dg-options "-fdiagnostics-show-caret -fdiagnostics-show-line-numbers" } */

void test (int i, int j)
{
  printf ("%i of %i\n", i, j); /* { dg-line printf } */
  /* { dg-error "implicit declaration of function" "" { target *-*-* } printf } */
  /* { dg-begin-multiline-output "" }
   10 |   printf ("%i of %i\n", i, j);
      |   ^~~~~~
   { dg-end-multiline-output "" } */
  /* { dg-warning "incompatible implicit declaration" "" { target *-*-* } printf } */
  /* { dg-begin-multiline-output "" }
   10 |   printf ("%i of %i\n", i, j);
      |   ^~~~~~
   { dg-end-multiline-output "" } */
  /* { dg-message "include '<stdio.h>' or provide a declaration of 'printf'" "" { target *-*-* } 4 } */
  /* { dg-begin-multiline-output "" }
    3 | #include "empty.h"
  +++ |+#include <stdio.h>
    4 | int the_next_line;
   { dg-end-multiline-output "" } */
}
