void foo(double bar[], double *zp, int n)
{
   int i, j;

   i = 0;
   for(j = 0; j < n; j++)
   {
      i += j+1;
      bar[i] *= (1.0 + *zp);
   }
}
