/* Test that __STDC__ is not defined.  */

/* { dg-do preprocess } */

#if defined __STDC__
# error __STDC__ defined /* { dg-bogus "__STDC__" "__STDC__ defined" } */
#endif
