/* Example of a fix-it hint that adds a #include directive,
   adding them to the top of the file, given that there is no
   pre-existing #include.  */

/* { dg-options "-fdiagnostics-show-caret -fdiagnostics-show-line-numbers -Wno-implicit-function-declaration" } */

void test (int i, int j)
{
  printf ("%i of %i\n", i, j); /* { dg-warning "implicit declaration" } */
  /* { dg-message "include '<stdio.h>' or provide a declaration of 'printf'" "" { target *-*-* } 1 } */
#if 0
/* { dg-begin-multiline-output "" }
    9 |   printf ("%i of %i\n", i, j);
      |   ^~~~~~
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
  +++ |+#include <stdio.h>
    1 | /* Example of a fix-it hint that adds a #include directive,
   { dg-end-multiline-output "" } */
#endif
}
