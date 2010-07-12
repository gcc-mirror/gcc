/* { dg-do compile } */
/* { dg-options "-O -fprefetch-loop-arrays -w" } */
/* { dg-options "-O -fprefetch-loop-arrays -march=i686 -msse -w" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-require-effective-target sse { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

void foo(int);
void bar(int n)
{
 int a[2], i, j = 0;

 for (i = 0; i < 2; i += j+1)
   for (j = 0; j < (n ? 1 : 2); ++j)
     foo(a[i] + a[j]);
}
