/* { dg-do preprocess } */

/* Test the various unary operators.  */

#if 1 + + + 1   /* allow multiple unary sign operators :) */
#endif

#if 8 - +3 != +4 + +1
#error		/* { dg-bogus "error" "unary +" } */
#endif

#if -2 - -1 != -1
#error		/* { dg-bogus "error" "unary -" } */
#endif

#if ~0 != -1
#error		/* { dg-bogus "error" "unary ~" } */
#endif

#if !0 && (!1 == 0) && !!1 != 1
#error		/* { dg-bogus "error" "unary !" } */
#endif

#if ~~8 != 8
#error		/* { dg-bogus "error" "double unary ~" } */
#endif

#if 5 + +!-4 != 5
#error		/* { dg-bogus "error" "compound unary +, !, -" } */
#endif
