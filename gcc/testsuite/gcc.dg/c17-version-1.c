/* Test __STDC_VERSION__ for C17.  Test -std=c17.  */
/* { dg-do compile } */
/* { dg-options "-std=c17 -pedantic-errors" } */

#if __STDC_VERSION__ == 201710L
int i;
#else
#error "Bad __STDC_VERSION__."
#endif
