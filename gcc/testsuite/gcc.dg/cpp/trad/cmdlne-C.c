/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-C -traditional-cpp" } */

/* Test -C doesn't fail with #define.  #define is the tricky case,
   being the only directive that remembers its comments.

   -C treats comments as tokens in their own right, so e.g. comment at
   the beginning of a directive turns it into a non-directive.  */

#define simple no comments

#define/**/obj_like/**/(some)/**/thing/**/
#define fun_like(/**/x/**/,/**/y/**/)/**/
/**/#define not_a_macro

#if !defined simple || !defined obj_like || !defined fun_like
#error Missed some macros with -C
#endif

#ifdef not_a_macro
#error not_a_macro is!
#endif

/* Check obj_like doesn't expect arguments, and fun_like does.  */
obj_like
fun_like (foo, bar)

/* Check OK to redefine fun_like without comments in the params.  */
#define fun_like(x, y)/**/

/* Check comments in macros in directives are OK.  */
#define ZERO 0 /* A trailing comment.  */

#if ZERO
#endif
