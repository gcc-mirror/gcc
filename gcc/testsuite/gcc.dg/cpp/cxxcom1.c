/* { dg-do preprocess } */
/* { dg-options "-pedantic -std=gnu89" } */

/* You can't do this in your own code... */
// C++ comment is not in C89  { dg-warning "style comment|reported only once" "good warning" }

/* ...but we don't bitch about it more than once.  */
// C++ comment is not in C89  { dg-bogus "style comment" "bad warning" }

/* { dg-final { scan-file-not cxxcom1.i "is not in C89" } } */

