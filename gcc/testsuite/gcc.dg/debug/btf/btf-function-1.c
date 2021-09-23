/* Test BTF generation for functions.

   We expect to see one BTF_KIND_FUNC_PROTO with 2 named arguments.
   The parameter names should appear in the auxilliary string table.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0xd000002\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times "farg_name" 2 } } */
/* { dg-final { scan-assembler-times "farg_type" 2 } } */
/* { dg-final { scan-assembler-times "ascii \"alpha.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */
/* { dg-final { scan-assembler-times "ascii \"bravo.0\"\[\t \]+\[^\n\]*btf_aux_string" 1 } } */

int funfoo (int alpha, long bravo)
{
  return 0;
}
