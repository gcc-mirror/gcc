/* { dg-do preprocess } */
/* { dg-options "-CC" } */

/* This tests to make sure that comments are ignored between # and the
   directive name when the -CC option is used.

   Jason R. Thorpe, 6 Apr 2002  */

#/**/define def passed

def

/* { dg-final { scan-file maccom1.i "(^|\\n)passed" } } */

