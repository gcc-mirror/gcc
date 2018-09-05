/* PR tree-optimization/87072 - g++6.2.0 false warning: array subscript
   is above array bounds, with misleading line number
   { dg-do compile }
   { dg-options "-O3 -Wall" }  */

int a[10];

void f (unsigned n)
{
  for (unsigned j = 0; j < n; j++) {
     for (unsigned k = 0; k < j; k++)
       a[j] += k;                       /* { dg-bogus "\\\[-Warray-bounds]" } */
     a[j] += j;
  }
}
