/* Verify zero initialization for floating point type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -fdump-tree-gimple" } */

long double result;

long double foo()
{
  float temp1;
  double temp2;
  long double temp3;
  
  result = temp1 + temp2 + temp3;
  return result;
}

/* { dg-final { scan-tree-dump "temp1 = .DEFERRED_INIT \\(4, 2, \&\"temp1\"" "gimple" } } */
/* { dg-final { scan-tree-dump "temp2 = .DEFERRED_INIT \\(8, 2, \&\"temp2\"" "gimple" } } */
/* { dg-final { scan-tree-dump "temp3 = .DEFERRED_INIT \\((8|12|16), 2, \&\"temp3\"" "gimple" } } */
