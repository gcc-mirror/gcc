/* { dg-do preprocess } */

/* Based on Debian GNATS PR 8524.  17 Nov 2002.  */

#define ALPHA(A) alpha_ ## A
#define BETA(B) beta_ ## B
#define GAMMA(C) _Pragma("moose") ALPHA(C) BETA(C)
GAMMA(baz);

/* { dg-final { scan-file "_Pragma5.i" "alpha_baz beta_baz;" } } */
