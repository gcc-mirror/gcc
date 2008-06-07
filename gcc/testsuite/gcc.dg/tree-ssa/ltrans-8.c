/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-linear -fdump-tree-ltrans-all" } */
double foo(double *a)
{
       int i,j;
       double r = 0.0;
      for (i=0; i<100; ++i)
               for (j=0; j<1000; ++j)
                      r += a[j*100+i];
       return r;
}

/* { dg-final { scan-tree-dump-times "transformed loop" 1 "ltrans"} } */ 
/* { dg-final { cleanup-tree-dump "ltrans" } } */
