/* Test for some basic aspects of -traditional directive processing.  */

/* { dg-do preprocess } */
/* { dg-options -traditional } */

/* There is no #error directive.  */

#error bad	/* { dg-bogus "bad" } */

/* Directives with their #s indented are not recognized.  */
 #if 0	/* { dg-bogus "unterminated" } */
