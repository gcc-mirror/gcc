/* { dg-do compile } */
/* { dg-options "-w -O2 -mdejagnu-cpu=power9" } */

/* PR92661: The following tests should not ICE, regardless of
   whether the target supports DFP or not.  */

/* Test that a normal builtin function doesn't ICE.  */
int
foo (_Decimal64 src) /* { dg-error "decimal floating-point not supported for this target" "not supported" { target { ! dfp } } } */
{
  return __builtin_dfp_dtstsfi_lt_dd (5, src);
}

/* Test that an overloaded builtin function doesn't ICE.  */
int
bar (_Decimal64 src) /* { dg-error "decimal floating-point not supported for this target" "not supported" { target { ! dfp } } } */
{
  return __builtin_dfp_dtstsfi_lt (5, src);
}
