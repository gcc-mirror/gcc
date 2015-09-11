/* PR preprocessor/60570 */
/* { dg-do preprocess } */

#if 1
int i;
#elif 1/0
#endif

#if 1
int j;
#elif
#endif

#if 0
#elif 1/0	/* { dg-error "division by zero" } */
int k;
#endif

#if 0
#elif		/* { dg-error "with no expression" } */
int n;
#endif

#if 1
# if 1
int l;
# elif 1/0
# endif
#endif

#if 1
# if 1
int l;
# elif
# endif
#endif

#if 1
# if 0
# elif 1/0	/* { dg-error "division by zero" } */
# endif
#endif

#if 1
# if 0
# elif		/* { dg-error "with no expression" } */
# endif
#endif
