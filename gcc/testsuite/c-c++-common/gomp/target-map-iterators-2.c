/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

void f (int *x, float *y, double *z)
{
  #pragma omp target map(iterator(i=0:10), to: x) /* { dg-warning "iterator variable 'i' not used in clause expression" } */
    /* Add a reference to x to ensure that the 'to' clause does not get
       dropped.  */
    x[0] = 0;

  #pragma omp target map(iterator(i2=0:10, j2=0:20), from: x[i2]) /* { dg-warning "iterator variable 'j2' not used in clause expression" } */
    ;

  #pragma omp target map(iterator(i3=0:10, j3=0:20, k3=0:30), to: x[i3+j3], y[j3+k3], z[k3+i3])
  /* { dg-warning "iterator variable 'i3' not used in clause expression" "" { target *-*-* } .-1 } */
  /* { dg-warning "iterator variable 'j3' not used in clause expression" "" { target *-*-* } .-2 } */
  /* { dg-warning "iterator variable 'k3' not used in clause expression" "" { target *-*-* } .-3 } */
    ;

  /* Test iterator with zero iterations.  */
  #pragma omp target map(iterator(i4=0:0), to: x[i4]) /* { dg-warning "iteration count is zero" } */
    ;

  /* Test iterator where the beginning is greater than the end.  */
  #pragma omp target map(iterator(i5=10:0), to: x[i5]) /* { dg-warning "iteration count is zero" } */
    ;

  /* Test iterator where the beginning is greater than the end, but with a
     negative step.  */
  #pragma omp target map(iterator(i6=10:0:-1), to: x[i6])
    ;
}

/* { dg-final { scan-tree-dump-times "map\\\(to:x" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\\(iterator\\\(int i2=0:10:1, loop_label=\[^\\\)\]+\\\):from:" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\\(iterator\\\(int i3=0:10:1, int j3=0:20:1, loop_label=\[^\\\)\]+\\\):to:" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\\(iterator\\\(int j3=0:20:1, int k3=0:30:1, loop_label=\[^\\\)\]+\\\):to:" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\\(iterator\\\(int i3=0:10:1, int k3=0:30:1, loop_label=\[^\\\)\]+\\\):to:" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\\(iterator\\\(int i4=0:0:1, loop_label=\[^\\\)\]+\\\):to:" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\\(iterator\\\(int i5=10:0:1, loop_label=\[^\\\)\]+\\\):to:" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\\(iterator\\\(int i6=10:0:-1, loop_label=\[^\\\)\]+\\\):to:" 1 "gimple" } } */
