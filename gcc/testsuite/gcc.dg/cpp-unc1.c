/* Tests for un-terminated conditionals: 1.  */

#if 1  /* { dg-error "unterminated" "unterminated #if" } */

#ifdef notdef /* { dg-bogus "unterminated" "nested terminated #ifdef" } */

blah blah blah

#endif

more text

/* oops! */
