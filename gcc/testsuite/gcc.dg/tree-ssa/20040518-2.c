/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-phiopt1-details" } */

int t( int i)
{
   int j;
   if(i>=0)
    j = i;
   else
    j = -i;
   return j;
}

/* We should convert one COND_EXPRs into straightline code with ABS.  */
/* { dg-final { scan-tree-dump-times "straightline" 1 "phiopt1"} } */
/* { dg-final { scan-tree-dump-times "ABS_EXPR" 1 "phiopt1"} } */
/* { dg-final { cleanup-tree-dump "phiopt1" } } */
