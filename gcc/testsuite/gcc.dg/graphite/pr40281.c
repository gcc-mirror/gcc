/* { dg-do compile } */
/* { dg-options "-O -fprefetch-loop-arrays" } */

void foo(int);
void bar(int n)
{
 int a[2], i, j = 0;

 for (i = 0; i < 2; i += j+1)
   for (j = 0; j < (n ? 1 : 2); ++j)
     foo(a[i] + a[j]);
}
