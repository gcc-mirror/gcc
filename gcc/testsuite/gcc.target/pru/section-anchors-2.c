/* Ensure section anchors are enabled by default.  */

/* { dg-do compile } */
/* { dg-options "-O1" } */

int aa;
int bb;

int
test (void)
{
  return aa + bb;
  /* { dg-final { scan-assembler {\n\tldi32\tr\d+, \.LANCHOR\d+} } } */
}
