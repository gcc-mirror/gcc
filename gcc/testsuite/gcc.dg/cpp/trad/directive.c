/* Test for some basic aspects of -traditional directive processing.  */

/* { dg-do preprocess } */

/* There is a #error directive.  */

#error bad	/* { dg-error "bad" } */

/* Directives with their #s indented are not recognized.  */
 #if 0	/* { dg-bogus "unterminated" } */

#wrong	/* { dg-error "invalid" } */
