/* Test :: token in C23: preprocessed output.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c23 -pedantic-errors -P" } */

#define COLON() :
#define TEST() ABC

/* This must have a space inserted between the two ':' tokens in
   preprocessed output.  */
TEST()COLON()COLON()TEST()
/* { dg-final { scan-file c23-scope-2.i "ABC: :ABC" } } */
