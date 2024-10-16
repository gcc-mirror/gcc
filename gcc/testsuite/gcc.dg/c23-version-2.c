/* Test __STDC_VERSION__ for C23.  Test -std=iso9899:2024.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:2024 -pedantic-errors" } */

#if __STDC_VERSION__ == 202311L
int i;
#else
#error "Bad __STDC_VERSION__."
#endif
