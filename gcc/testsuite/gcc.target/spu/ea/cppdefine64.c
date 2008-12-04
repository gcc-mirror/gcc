/* { dg-options "-std=gnu89 -mea64" } */
/* { dg-do compile } */

#ifdef __EA64__
int x;
#else
#error __EA64__ undefined
#endif
