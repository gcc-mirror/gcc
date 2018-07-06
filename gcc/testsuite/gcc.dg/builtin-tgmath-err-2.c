/* Test __builtin_tgmath: errors that indicate bad arguments in a call
   to a type-generic macro, non-DFP.  */
/* { dg-do compile } */
/* { dg-options "" } */

float f_f (float);
double f_d (double);
long double f_ld (long double);
void *p;
long double ld;
_Complex float cf;

void
test (void)
{
  __builtin_tgmath (f_f, f_d, f_ld, p); /* { dg-error "invalid type of argument" } */
  __builtin_tgmath (f_f, f_d, ld); /* { dg-error "no matching function for type-generic call" } */
  __builtin_tgmath (f_f, f_d, cf); /* { dg-error "no matching function for type-generic call" } */
}
