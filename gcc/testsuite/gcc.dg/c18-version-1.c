/* Test __STDC_VERSION__ for C17.  Test -std=c18.  */
/* { dg-do compile } */
/* { dg-options "-std=c18 -pedantic-errors" } */

#if __STDC_VERSION__ == 201710L
int i;
#else
#error "Bad __STDC_VERSION__."
#endif
