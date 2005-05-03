/* Test compiler crash when dependence analyzer can not represent
   dependence relation by distance vector.  */
/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int x[199];

void foo()
  
{
  int t,j;

  for (j=99;j>0;j--) 
    x [j+j]=x[j];

  for (j=198;j>=100;j--) 
    if(x[j]) 
      {
	x[j-63]=x[j-3]-x[j];
      }
}
/* { dg-final { cleanup-tree-dump "vect" } } */
