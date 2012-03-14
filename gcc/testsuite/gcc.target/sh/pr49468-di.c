/* Check that 64 bit integer abs is generated as negc instruction pairs
   and conditional branch instead of default branch-free code.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-times "negc" 4 } } */


/* Normal integer absolute value.  */
long long
abs_0 (long long i)
{
  return (i < 0) ? -i : i;
}

/*  Negated integer absolute value.
    The generated code should be the same, except that the branch 
    condition is inverted.  */
long long
abs_1 (long long i)
{
  return (i > 0) ? -i : i;
}
