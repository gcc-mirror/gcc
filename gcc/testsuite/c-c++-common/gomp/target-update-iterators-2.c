/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

void f (int *x, float *y, double *z)
{
  #pragma omp target update to(iterator(i=0:10): x) /* { dg-warning "iterator variable 'i' not used in clause expression" }*/
    ;

  #pragma omp target update from(iterator(i2=0:10, j2=0:20): x[i2]) /* { dg-warning "iterator variable 'j2' not used in clause expression" }*/
    ;

  #pragma omp target update to(iterator(i3=0:10, j3=0:20, k3=0:30): x[i3+j3], y[j3+k3], z[k3+i3])
  /* { dg-warning "iterator variable 'i3' not used in clause expression" "" { target *-*-* } .-1 } */
  /* { dg-warning "iterator variable 'j3' not used in clause expression" "" { target *-*-* } .-2 } */
  /* { dg-warning "iterator variable 'k3' not used in clause expression" "" { target *-*-* } .-3 } */
    ;
}

/* { dg-final { scan-tree-dump "update to\\\(x " "gimple" } } */
/* { dg-final { scan-tree-dump "update from\\\(iterator\\\(int i2=0:10:1, loop_label=" "gimple" } } */
/* { dg-final { scan-tree-dump "to\\\(iterator\\\(int i3=0:10:1, int k3=0:30:1, loop_label=" "gimple" } } */
/* { dg-final { scan-tree-dump "to\\\(iterator\\\(int j3=0:20:1, int k3=0:30:1, loop_label=" "gimple" } } */
/* { dg-final { scan-tree-dump "to\\\(iterator\\\(int i3=0:10:1, int j3=0:20:1, loop_label=" "gimple" } } */
