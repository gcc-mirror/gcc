/* Ensure section anchors are enabled by default.  */

/* { dg-do assemble } */
/* { dg-options "-O1" } */
/* { dg-final { object-size text == 24 } } */

int aa;
int bb;

int
test (void)
{
  return aa + bb;
}
