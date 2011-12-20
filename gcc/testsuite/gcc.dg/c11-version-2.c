/* Test __STDC_VERSION__ for C11.  Test -std=iso9899:2011.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:2011 -pedantic-errors" } */

#if __STDC_VERSION__ == 201112L
int i;
#else
#error "Bad __STDC_VERSION__."
#endif
