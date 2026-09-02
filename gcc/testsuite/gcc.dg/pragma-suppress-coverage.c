/* Verify that we use emit diagnostics for #pragma GCC suppress_coverage.  */

/* { dg-do assemble } */
/* { dg-options "-fdiagnostics-show-caret" } */

#pragma GCC suppress_coverage end
/* { dg-warning "no matching begin for '#pragma GCC suppress_coverage end'" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma GCC suppress_coverage end
         ^~~
   { dg-end-multiline-output "" }  */

#pragma GCC suppress_coverage
/* { dg-warning "'#pragma GCC suppress_coverage' must be followed by 'begin' or 'end'" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma GCC suppress_coverage
         ^~~
   { dg-end-multiline-output "" }  */

#pragma GCC suppress_coverage begin more
/* { dg-warning "junk at end of '#pragma GCC suppress_coverage'" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma GCC suppress_coverage begin more
                                     ^~~~
   { dg-end-multiline-output "" }  */

/* Pushing multiple suppresions is fine, this works like a stack.  It should
   not trigger a warning because this could easily happen with recursive
   expansions, for example when some assert-like macro with internal suppression
   is used in a suppressed context.  */
#define suppress_push() _Pragma("GCC suppress_coverage begin")
#define suppress_pop() _Pragma("GCC suppress_coverage end")

suppress_push()
suppress_pop()

#pragma GCC suppress_coverage end

#pragma GCC suppress_coverage end
/* { dg-warning "no matching begin for '#pragma GCC suppress_coverage end'" "" { target *-*-* } .-1 }
   { dg-begin-multiline-output "" }
 #pragma GCC suppress_coverage end
         ^~~
   { dg-end-multiline-output "" }  */
