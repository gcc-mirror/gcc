/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Source: Neil Booth.  */

/* Various illegal expressions with missing components.  */

#if		/* { dg-error "no expression" "empty #if" } */
#endif

#if ~		/* { dg-error "no right op" "no unary operand" } */
#endif

#if 3 + * 6 + 4  /* { dg-error "no right op" "no right operand" } */
#endif

#if 2 ~2	/* { dg-error "missing bin" "no binary operator" } */
#endif

#if 1 + 2 (3)   /* { dg-error "missing bin" "immediate then open paren" } */
#endif

#if (2) 4 * 2  /* { dg-error "missing bin" "close paren then immediate" } */
#endif

#if == 2  /* { dg-error "no left op" } */
#endif

#if (==2)  /* { dg-error "no left op" } */
#endif
