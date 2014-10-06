/* { dg-do compile } */
/* { dg-options "-O0 -gdwarf -dA -fgnu89-inline" } */
void e(int);
__attribute__ ((always_inline)) inline int
t(int function_parameter)
{
  e(function_parameter);
}

void test (int function_parameter2)
{
  t(function_parameter2);
}

/* Verify that we get both function_parameter and function_parameter2 declared
   in test.  Overall we should have 3 variables with location defined (also
   function_parameter in offline copy of t.  */
/* { dg-final { scan-assembler-times " DW_AT_location" 3 } } */
