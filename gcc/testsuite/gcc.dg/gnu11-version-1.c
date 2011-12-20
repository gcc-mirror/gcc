/* Test __STDC_VERSION__ for C11 with GNU extensions.  Test -std=gnu11.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu11 -pedantic-errors" } */

#if __STDC_VERSION__ == 201112L
int i;
#else
#error "Bad __STDC_VERSION__."
#endif
