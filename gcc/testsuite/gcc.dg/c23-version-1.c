/* Test __STDC_VERSION__ for C23.  Test -std=c23.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#if __STDC_VERSION__ == 202311L
int i;
#else
#error "Bad __STDC_VERSION__."
#endif
