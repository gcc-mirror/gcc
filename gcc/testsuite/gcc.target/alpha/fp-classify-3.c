/* With -fsignaling-nans the comparisons are unusable even with -mieee,
   since cmptun raises an invalid-operation exception for a signaling NaN.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mieee -fsignaling-nans" } */

int isnan_f (float x) { return __builtin_isnan (x); }
int isnan_d (double x) { return __builtin_isnan (x); }

/* { dg-final { scan-assembler-not "\tcmpt" } } */
