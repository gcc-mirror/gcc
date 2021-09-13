/* Test #elifdef and #elifndef not in C11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#define A
#undef B

#if 0
#elifdef A
#error "#elifdef A applied"
#endif

#if 0
#elifndef B
#error "#elifndef B applied"
#endif
