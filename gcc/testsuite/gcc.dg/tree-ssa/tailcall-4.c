/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-tailc-details" } */
/* PR tree-opt/37024 */

double doubleValue();

long
longValue ()
{
 return (long) doubleValue ();
}

/* We should not tail call doubleValue in longValue as the mode changes. */
/* { dg-final { scan-tree-dump-times "Found tail call" 0 "tailc"} } */
/* { dg-final { cleanup-tree-dump "tailc" } } */

