/* Test #elifdef and #elifndef in C23.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#define A
#undef B

#if 0
#elifdef A
#define M1 1
#endif

#if M1 != 1
#error "#elifdef A did not apply"
#endif

#if 0
#elifdef B
#error "#elifdef B applied"
#endif

#if 0
#elifndef A
#error "#elifndef A applied"
#endif

#if 0
#elifndef B
#define M2 2
#endif

#if M2 != 2
#error "#elifndef B did not apply"
#endif

#if 0
#elifdef A
#else
#error "#elifdef A did not apply"
#endif

#if 0
#elifndef B
#else
#error "#elifndef B did not apply"
#endif

/* As with #elif, the syntax of the new directives is relaxed after a
   non-skipped group.  */

#if 1
#elifdef x * y
#endif

#if 1
#elifndef !
#endif
