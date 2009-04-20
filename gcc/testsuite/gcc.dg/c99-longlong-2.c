/* Test for long long: if explicit Wlong-long, in C99 only warn, not
   pedwarn.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors -Wlong-long" } */

long long foo; /* { dg-warning "long long" } */
