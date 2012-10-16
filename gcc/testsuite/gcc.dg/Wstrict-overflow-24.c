/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2" } */
/* { dg-message "warnings being treated as errors" "" {target "*-*-*"} 0 } */
#pragma GCC diagnostic error "-Wstrict-overflow"

int
foo (int i)
{
  return __builtin_abs (i) >= 0; /* { dg-error "assuming signed overflow does not occur" "correct warning" } */
}
