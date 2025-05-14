/* PR120003 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce3-details" } */

extern _Bool g(int);

_Bool f()
{
  _Bool retval = 0;
  for(int i=0; i<1000000; ++i)
    retval = retval || g(i);
  return retval;
}

/* Jump threading after loop optimization should get the counting loop
   separated from the loop until retval is true and CD-DCE elide it.
   It's difficult to check for the fact that a true retval terminates
   the loop so check CD-DCE eliminates one loop instead.  */
/* { dg-final { scan-tree-dump "fix_loop_structure: removing loop" "cddce3" } } */
