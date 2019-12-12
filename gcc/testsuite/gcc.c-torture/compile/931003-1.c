/* { dg-require-stack-size "52*8" } */

f (n, a)
     int n;
     double a[];
{
   double b[51];
   int i, j;

   i = 0;

   for (j = n - 1; j > 0; j--)
     b[i++] = 0;

   if (b[0] > b[i - 1])
     a[i] = b[i - 1];
}
