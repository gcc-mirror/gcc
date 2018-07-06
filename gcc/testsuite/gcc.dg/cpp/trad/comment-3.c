/* Test we don't accept C++ comments.  */

/* { dg-do preprocess } */

#if 0
#endif //  /* { dg-warning "-:extra tokens" } */
