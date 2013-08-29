/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ivcanon-details" } */
int a[1];
test(int c)
{ 
  int i=0,j;
  for (i=0;i<c;i++)
    {
      for (j=0;j<c;j++)
	{
          a[i]=5;
	  test2();
	}
    }
}

/* We should do this as part of cunrolli, but our cost model do not take into account early exit
   from the last iteration.  */
/* { dg-final { scan-tree-dump "loop turned into non-loop; it never loops." "ivcanon"} } */
/* { dg-final { scan-tree-dump "Last iteration exit edge was proved true." "ivcanon"} } */
/* { dg-final { cleanup-tree-dump "ivcanon" } } */
