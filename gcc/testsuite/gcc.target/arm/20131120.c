/* Check that CONST_INT is not forced into REG before PLUS.  */
/* { dg-do compile { target { arm_arm_ok || arm_thumb2_ok} } } */
/* { dg-options "-O2 -fdump-rtl-expand" } */

typedef int Arr2[50][50];

void
foo (Arr2 a2, int i)
{
  a2[i+20][i] = 1;
}

/* { dg-final { scan-rtl-dump-not "\\\(set \\\(reg:SI \[0-9\]*\\\)\[\n\r\]+\[ \t]*\\\(const_int 4000" "expand" } } */
