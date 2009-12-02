/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution" } */

extern int A[];
extern int B[];

void f(int i)
{
   while (i-- > 0) {
     A[i] = 0;
     B[i] = 0;
   }
}
