/* Check that zero-displacement branches are used instead of branch-free
   execution patterns.
   This is usually handled by the *cset_zero patterns.  */
/* { dg-do compile }  */
/* { dg-options "-O1 -mzdcbranch" }  */
/* { dg-final { scan-assembler-not "subc|and|bra" } }  */
/* { dg-final { scan-assembler-times "bf\t0f" 1 } }  */
/* { dg-final { scan-assembler-times "bt\t0f" 1 } }  */

int*
test_00 (int* s)
{
  if (s[0] == 0)
    if (!s[3])
      s = 0;
  return s;
}

int*
test_01 (int* s)
{
  if (s[0] == 0)
    if (s[3])
      s = 0;
  return s;
}
