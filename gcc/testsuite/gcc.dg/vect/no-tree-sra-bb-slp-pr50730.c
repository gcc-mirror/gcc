/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

typedef __complex__ float Value;
typedef struct {
  Value a[16 / sizeof (Value)];
} A;

A sum(A a,A b)
{
  a.a[0]+=b.a[0];
  a.a[1]+=b.a[1];
  return a;
}

/* { dg-final { scan-tree-dump-times "basic block vectorized" 1 "slp2" } } */
