/* Technically a bug, won't fix, probably.

   OpenMP 6.0 (158:18)
   In all cases, white space in clause-argument-list is optional.
   
   The lexer obviously doesn't like this very much, but technically it is
   correct OpenMP syntax, the first colon is a part of the
   modifier-specification-list, the second is a numeric range with both
   lb and ub not specified.  */

void v (int) {}

#pragma omp declare variant(v) match(construct={dispatch}) \
			       adjust_args(nothing::) /* { dg-bogus "" "" { xfail *-*-* } } */
void b (int) {}
