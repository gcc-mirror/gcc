/* Verify zero initialization for complex type automatic variables.  */
/* { dg-do compile } */
/* { dg-options "-ftrivial-auto-var-init=zero -fdump-tree-gimple" } */


_Complex long double result;

_Complex long double foo()
{
  _Complex float temp1;
  _Complex double temp2;
  _Complex long double temp3;

  result = temp1 + temp2 + temp3;
  return result;
}

/* { dg-final { scan-tree-dump "temp1 = .DEFERRED_INIT \\(8, \[26], \&\"temp1\"" "gimple" } } */
/* { dg-final { scan-tree-dump "temp2 = .DEFERRED_INIT \\(16, \[26], \&\"temp2\"" "gimple" } } */
/* { dg-final { scan-tree-dump "temp3 = .DEFERRED_INIT \\((16|24|32), \[26], \&\"temp3\"" "gimple" } } */

