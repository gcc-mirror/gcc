// PR c++/84686
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-times "= i" 10 "gimple" } }

volatile int i;

int main()
{
  i; //evaluated (a load is performed)
  (i); //unevaluated => the load shall be performed

  (void)i; //evaluated (a load is performed)
  (void)(i); //unevaluated => the load shall be performed

  (void)i; //evaluated (a load is performed)
  (void)(i); //unevaluated => the load shall be performed

  (i,i); // the two subexpression are evaluated
  ((i),(i)); // no evaluation, => two loads shall happen
}
