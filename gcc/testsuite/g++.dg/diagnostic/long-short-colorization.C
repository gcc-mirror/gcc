// Verify colorization of printing of declspec mismatches
// Use dg-*-multiline-output to avoid regexp interpretation.

// { dg-options "-fdiagnostics-color=always -fdiagnostics-show-caret" }

long short int a;
/* { dg-begin-multiline-output "" }
[m[K'[01m[K[01;32m[Klong[m[K[m[K' and '[01m[K[01;34m[Kshort[m[K[m[K' specified together
 [01;32m[Klong[m[K [01;34m[Kshort[m[K int a;
 [01;32m[K^~~~[m[K [01;34m[K~~~~~[m[K
   { dg-end-multiline-output "" } */
short long int b;
/* { dg-begin-multiline-output "" }
[m[K'[01m[K[01;32m[Klong[m[K[m[K' and '[01m[K[01;34m[Kshort[m[K[m[K' specified together
 [01;34m[Kshort[m[K [01;32m[Klong[m[K int b;
 [01;34m[K~~~~~[m[K [01;32m[K^~~~[m[K
   { dg-end-multiline-output "" } */

// Discard the remaining colorized output that confuses dejagnu.
// { dg-prune-output diagnostic/long-short-colorization.C }
