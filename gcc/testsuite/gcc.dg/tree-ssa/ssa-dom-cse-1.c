/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-dom2-details" } */
int t(int a) __attribute__ ((const));
void q (void);
void
threading(int a,int b)
{
	if (t(a))
	{
	  if (t(a))
	    q();
	}
}
/* We should thread the jump twice and eliminate it.  Test this in
   DOM2, after aliases have been computed.  */
/* { dg-final { scan-tree-dump-times "Replaced.* t " 1 "dom2"} } */
