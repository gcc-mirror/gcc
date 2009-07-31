/* Formerly known as ltrans-8.c */

double foo(double *a)
{
       int i,j;
       double r = 0.0;
      for (i=0; i<100; ++i)
               for (j=0; j<1000; ++j)
                      r += a[j*100+i];
       return r;
}

/* { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" } } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */
