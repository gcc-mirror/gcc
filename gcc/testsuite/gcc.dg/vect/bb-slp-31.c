/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

typedef double vec __attribute__ ((vector_size (2 * sizeof (double))));
vec a;

void f(){
  a[0]=1+2*a[0]*a[0];
  a[1]=1+2*a[1]*a[1];
}

/* { dg-final { scan-tree-dump "basic block vectorized using SLP" "slp" } } */
