/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-linear" } */

extern int s;

void
foo (int *x, int y, int z)
{
 int m, n;
 int o;
 int p = x[0];
 o = s;
 for (m = 0; m < s; m++)
   for (n = 0; n < s; n++)
     {
       if (x[n] != p)
         continue;
       if (m > z)
         z = m;
       if (n < o)
         o = n;
     }
 for (m = y; m <= z; m++)
   {
   }
}
