/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-dom1-details" } */
void t(void);
void q(void);
void q1(void);
void
threading(int a,int b)
{
	if (a>b)
	  t();
	else
	  q();
	if (a<=b)
	  q1();
}
/* We should thread the jump twice and elliminate it.  */
/* { dg-final { scan-tree-dump-times "Threaded" 2 "dom1"} } */
