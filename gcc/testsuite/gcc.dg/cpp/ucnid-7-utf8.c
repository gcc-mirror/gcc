/* { dg-do compile } */
/* { dg-options "-std=c99" } */

/* When GCC reads UTF-8-encoded input into its internal UTF-8
representation, it does not apply any transformation to the data, and
in particular it makes no attempt to verify that the encoding is valid
UTF-8.  Historically, if any non-ASCII characters were found outside a
string or comment, they were treated as stray tokens and did not
necessarily produce an error, e.g. if, as in this test, they disappear
in the preprocessor.  Now that UTF-8 is also supported in identifiers,
the basic structure of this process has not changed; GCC just treats
invalid UTF-8 as a stray token.  This test verifies that the historical
behavior is unchanged.  In the future, if GCC were changed, say, to
validate the UTF-8 on input, then this test would no longer be
appropriate.  */


#define a b(
#define b(x) q
/* The line below contains invalid UTF-8.  */
int aœ);
