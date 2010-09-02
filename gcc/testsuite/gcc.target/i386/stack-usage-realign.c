/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-skip-if "no stack realignment" { *-*-darwin* } { "*" } { "" } } */
/* { dg-options "-fstack-usage -msse2 -mforce-drap" } */

typedef int __attribute__((vector_size(16))) vec;

vec foo (vec v)
{
  return v;
}

int main (void)
{
  vec V;
  V = foo (V);
  return 0;
}

/* { dg-final { scan-stack-usage "main\t48\tdynamic,bounded" } } */
/* { dg-final { cleanup-stack-usage } } */
