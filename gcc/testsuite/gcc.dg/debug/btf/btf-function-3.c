/* Test BTF generation for a function with an unrepresentable parameter.

   BTF has no encoding for floating point types, among others. Function
   parameters of unrepresentable types are emitted as 'void' types.

   We expect one BTF_KIND_FUNC_PROTO with 3 parameters, one of which
   has type_id=0.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA -Wno-psabi" } */

/* { dg-final { scan-assembler-times "\[\t \]0xd000003\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times "farg_name" 3 } } */
/* { dg-final { scan-assembler-times "farg_type" 3 } } */

/* Exactly one function parameter should have type_id=0.  */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*farg_type" 1 } } */

int foo (int a, float __attribute__((__vector_size__(16))) f, long b)
{
  return 0;
}
