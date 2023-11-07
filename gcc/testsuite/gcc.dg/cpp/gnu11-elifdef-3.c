/* Test #elifdef and #elifndef in GNU11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=gnu11 -pedantic" } */

#define A
#undef B

#if 0
#elifdef A	/* { dg-warning "#elifdef before C23 is a GCC extension" } */
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
#elifndef B	/* { dg-warning "#elifndef before C23 is a GCC extension" } */
#define M2 2
#endif

#if M2 != 2
#error "#elifndef B did not apply"
#endif

#if 0
#elifdef A	/* { dg-warning "#elifdef before C23 is a GCC extension" } */
#else
#error "#elifdef A did not apply"
#endif

#if 0
#elifndef B	/* { dg-warning "#elifndef before C23 is a GCC extension" } */
#else
#error "#elifndef B did not apply"
#endif

#if 1
#elifdef A	/* { dg-warning "#elifdef before C23 is a GCC extension" } */
#endif

#if 1
#elifndef B	/* { dg-warning "#elifndef before C23 is a GCC extension" } */
#endif

/* As with #elif, the syntax of the new directives is relaxed after a
   non-skipped group.  */

#if 1
#elifdef x * y	/* { dg-warning "#elifdef before C23 is a GCC extension" } */
#endif

#if 1
#elifndef !	/* { dg-warning "#elifndef before C23 is a GCC extension" } */
#endif
