/* { dg-do preprocess } */
/* { dg-options "-CC" } */

/* This tests to make sure that expressions function properly
   when used with macros containing comments and the -CC option
   is being used.

   Jason R. Thorpe, 6 Apr 2002  */

#define ONE   1 /* one */
#define TWO   2 /* two */
#define THREE 3 /* three */

#if (ONE + TWO) != THREE
failed
#else
passed
#endif

/* { dg-final { scan-file maccom6.i "(^|\n)passed" } } */
