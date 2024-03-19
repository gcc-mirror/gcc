
/* Forget to include any standard headers, all for built-in functions.
   Rely on -Wimplicit-function-declaration for fixit hints, not on
   -Wbuiltin-declaration-mismatch (which misses abs, isdigit, putchar).  */

/* { dg-options "-fpermissive -fdiagnostics-show-caret -fdiagnostics-show-line-numbers -Wimplicit-function-declaration -Wno-builtin-declaration-mismatch" } */

int
foo (char *m, int i)
{
  if (isdigit (m[0])) /* { dg-warning "implicit declaration of function" } */
  /* { dg-begin-multiline-output "" }
   11 |   if (isdigit (m[0]))
      |       ^~~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
  +++ |+#include <ctype.h>
    1 | 
     { dg-end-multiline-output "" } */
    {
      return abs (i); /* { dg-warning "implicit declaration of function" } */
  /* { dg-begin-multiline-output "" }
   21 |       return abs (i);
      |              ^~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
  +++ |+#include <stdlib.h>
    1 | 
     { dg-end-multiline-output "" } */
    }
  else
    putchar (m[0]); /* { dg-warning "implicit declaration of function" } */
  /* { dg-begin-multiline-output "" }
   32 |     putchar (m[0]);
      |     ^~~~~~~
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
  +++ |+#include <stdio.h>
    1 | 
     { dg-end-multiline-output "" } */
  return i;
}
