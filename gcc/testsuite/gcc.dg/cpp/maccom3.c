/* { dg-do preprocess } */
/* { dg-options "-CC" } */

/* This tests to make sure that comments in the definition of a macro
   parameter list are ignored when the -CC option is used.

   Jason R. Thorpe, 6 Apr 2002  */

#define def(x /**/, y) passed

def(x,y)

/* { dg-final { scan-file maccom3.i "(^|\n)passed" } } */
