/* { dg-do preprocess } */

/* Based on Debian GNATS PR 8524.  17 Nov 2002.  */

#define ALPHA(A) alpha_ ## A
#define BETA(B) beta_ ## B
#define GAMMA(C) _Pragma("moose") ALPHA(C) BETA(C)
GAMMA(baz);

/*
   { dg-final { if ![file exists _Pragma5.i] { return }                   } }
   { dg-final { if { [grep _Pragma5.i "alpha_baz beta_baz;"] != "" } { return }  } }
   { dg-final { fail "_Pragma5.c: _Pragma in macro"                       } }
*/
