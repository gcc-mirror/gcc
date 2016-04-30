/* Check that zero-displacement branches are used instead of branch-free
   execution patterns.  */
/* { dg-do compile }  */
/* { dg-options "-O1 -mzdcbranch" } */
/* { dg-final { scan-assembler-not "subc|and" } } */

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
