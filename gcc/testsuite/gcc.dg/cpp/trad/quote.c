/* Test that unterminated quotes are OK when only preprocessing.  */

/* { dg-do preprocess } */

/* { dg-bogus "unterminated" } */  'x
/* { dg-bogus "unterminated" } */  "x
