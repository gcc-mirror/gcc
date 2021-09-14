/* Verify pattern initialization for floating point type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=pattern -fdump-tree-gimple" } */

long double result;

long double foo()
{
  float temp1;
  double temp2;
  long double temp3;
  
  result = temp1 + temp2 + temp3;
  return result;
}

/* { dg-final { scan-tree-dump "temp1 = .DEFERRED_INIT \\(4, 1, 0\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "temp2 = .DEFERRED_INIT \\(8, 1, 0\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "temp3 = .DEFERRED_INIT \\((8|12|16), 1, 0\\)" "gimple" } } */
