/* { dg-options "-O2 --param modref-max-adjustments=8 -fdump-tree-modref1"  } */
/* { dg-do compile } */
void
set (char *p)
{
   p[1]=1;
   p[0]=0;
   p[2]=2;
   p[4]=4;
   p[3]=3;
}

void
recurse (char *p, int n)
{
	*p = 0;
	if (n)
	  recurse (p+1,n-1);
}
/* { dg-final { scan-tree-dump-not "param=modref-max-accesses" "modref1" } } */
/* { dg-final { scan-tree-dump "param=modref-max-adjustments" "modref1" } } */
/* In set all accesses should merge together.  */
/* { dg-final { scan-tree-dump "access: Parm 0 param offset:0 offset:0 size:8 max_size:40" "modref1" } } */
/* In recurse we should cap the recrusion after 8 attempts and set max_size to -1.  */
/* { dg-final { scan-tree-dump "access: Parm 0 param offset:0 offset:0 size:8 max_size:-1 adjusted 8 times" "modref1" } } */
