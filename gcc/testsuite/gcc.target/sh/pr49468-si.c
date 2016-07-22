/* Check that 32 bit integer abs is generated as neg instruction and
   conditional branch instead of default branch-free code.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler-times "neg" 2 } } */


/* Normal integer absolute value.  */
int
abs_0 (int i)
{
  return (i < 0) ? -i : i;
}

/*  Negated integer absolute value.
    The generated code should be the same, except that the branch 
    condition is inverted.  */
int
abs_1 (int i)
{
  return (i > 0) ? -i : i;
}
