/* { dg-do preprocess } */
/* { dg-options "-CC -traditional-cpp" } */

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

/*
   { dg-final { if ![file exists maccom6.i] { return }                    } }
   { dg-final { if { [grep maccom6.i "^passed"] != "" } { return }        } }
   { dg-final { fail "maccom6.c: comments in macro expressions with -CC"  } }
*/
