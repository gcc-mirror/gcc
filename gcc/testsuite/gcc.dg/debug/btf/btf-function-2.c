/* Test BTF generation for functions with varargs.

   We expect one BTF_KIND_FUNC_PROTO with two arguments. The second argument
   should have "farg_name" and "farg_type" both of 0, representing varargs.  */

/* { dg-do compile } */
/* { dg-options "-O0 -gbtf -dA" } */

/* { dg-final { scan-assembler-times "\[\t \]0xd000002\[\t \]+\[^\n\]*btt_info" 1 } } */
/* { dg-final { scan-assembler-times "farg_name" 2 } } */
/* { dg-final { scan-assembler-times "farg_type" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*farg_name" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]0\[\t \]+\[^\n\]*farg_type" 1 } } */

int fmt (const char * format, ...)
{
  return 0;
}
