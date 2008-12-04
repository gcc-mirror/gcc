/* Test default __EA32__ define.  */
/* { dg-options "-std=gnu89 -pedantic-errors -mea32" } */
/* { dg-do compile } */

#ifdef __EA32__
int x;
#else
#error __EA32__ undefined
#endif
