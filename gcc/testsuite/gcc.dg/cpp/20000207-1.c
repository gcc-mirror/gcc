/* { dg-do preprocess } */

/* Test for proper handling of unary minus in #if.  */

#if !(-1)
#error Error	/* { dg-bogus "Error" "case !(-1)" } */
#endif

#if !-1
#error Error	/* { dg-bogus "Error" "case !-1" } */
#endif

#if -1
#else
#error Error	/* { dg-bogus "Error" "case -1" } */
#endif
