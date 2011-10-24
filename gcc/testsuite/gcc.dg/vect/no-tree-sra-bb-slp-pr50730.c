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

/* { dg-final { scan-tree-dump-times "not vectorized: more than one data ref in stmt" 0 "slp" } } */
/* { dg-final { cleanup-tree-dump "slp" } } */
