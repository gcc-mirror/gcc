/* { dg-do preprocess } */
/* { dg-options "-CC" } */

/* This tests to make sure the comment is saved in the macro and copied
   to the output file when the macro is expanded when the -CC option is
   used.

   Jason R. Thorpe, 6 Apr 2002  */

#define def /* passed */

def

/*
   /* The + in the regexp prevents it from matching itself.  */
   { dg-final { scan-file maccom4.i "p+assed" } }
*/
