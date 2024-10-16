/* Test __STDC_VERSION__ for C23 with GNU extensions.  Test -std=gnu23.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23 -pedantic-errors" } */

#if __STDC_VERSION__ == 202311L
int i;
#else
#error "Bad __STDC_VERSION__."
#endif
