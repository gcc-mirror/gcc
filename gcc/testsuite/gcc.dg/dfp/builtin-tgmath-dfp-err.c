/* Test __builtin_tgmath: errors that indicate bad arguments in a call
   to a type-generic macro, DFP involved.  */
/* { dg-do compile } */
/* { dg-options "" } */

float f_f (float);
double f_d (double);
long double f_ld (long double);
_Complex float f_cf (_Complex float);
_Complex double f_cd (_Complex double);
_Complex long double f_cld (_Complex long double);
_Decimal32 f_d32 (_Decimal32);
_Decimal64 f_d64 (_Decimal64);
_Decimal128 f_d128 (_Decimal128);
float f_ff (float, float);
_Complex float f_cfcf (_Complex float, _Complex float);
_Decimal32 f_d32d32 (_Decimal32, _Decimal32);
_Complex float cf;
float f;
_Decimal32 d32;

void
test (void)
{
  __builtin_tgmath (f_cf, f_cd, f_cld, d32); /* { dg-error "decimal floating-point argument 1 to complex-only type-generic function" } */
  __builtin_tgmath (f_f, f_d, f_ld, d32); /* { dg-error "decimal floating-point argument 1 to binary-only type-generic function" } */
  __builtin_tgmath (f_cfcf, f_d32d32, cf, d32); /* { dg-error "both complex and decimal floating-point arguments to type-generic function" } */
  __builtin_tgmath (f_ff, f_d32d32, f, d32); /* { dg-error "both binary and decimal floating-point arguments to type-generic function" } */
  __builtin_tgmath (f_d32, f_d64, f_d128, cf); /* { dg-error "complex argument 1 to decimal-only type-generic function" } */
  __builtin_tgmath (f_d32, f_d64, f_d128, f); /* { dg-error "binary argument 1 to decimal-only type-generic function" } */
  __builtin_tgmath (f_cfcf, f_d32d32, d32, cf); /* { dg-error "both complex and decimal floating-point arguments to type-generic function" } */
  __builtin_tgmath (f_ff, f_d32d32, d32, f); /* { dg-error "both binary and decimal floating-point arguments to type-generic function" } */
}
