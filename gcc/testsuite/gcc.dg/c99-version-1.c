/* Test __STDC_VERSION__ for C99.  */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

#if __STDC_VERSION__ == 199901L
int i;
#else
#error "Bad __STDC_VERSION__."
#endif
