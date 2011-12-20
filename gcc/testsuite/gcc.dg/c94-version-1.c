/* Test __STDC_VERSION__ for C94.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:199409 -pedantic-errors" } */

#if __STDC_VERSION__ == 199409L
int i;
#else
#error "Bad __STDC_VERSION__."
#endif
