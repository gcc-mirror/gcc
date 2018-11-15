/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribute-patterns -ftrapv -fno-tree-fre" } */

void foo(int x[])
{
 int i, j;

 for (i = 0; i < 2; i++)
   for (j = 0; j < 2; j++)
   {
     x[i] = x[i*j];
     x[i] = x[i*j];
   }
}
