/* Test for some basic aspects of -traditional directive processing.  */

/* { dg-do preprocess } */

#define HASH #
HASH

/* There is a #error directive.  */

#error bad	/* { dg-error "bad" } */

/* Directives with their #s indented are not recognized.  */
 #if 0	/* { dg-bogus "unterminated" } */

#wrong	/* { dg-error "invalid" } */

#define foo 2
#define bar + 3
#define foobar 6

#if foo/**/bar != 5
# error Comments in directive is a separator /* { dg-bogus "error" } */
#endif
