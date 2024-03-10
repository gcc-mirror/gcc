/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "test:.*xor.*maskeqz.*masknez.*or.*" } } */

extern void foo_ii (int *, int *, int *, int *);

int
test (void)
{
  int a, b;
  int c, d, out;
  foo_ii (&a, &b, &c, &d);
  out = a == b ? c : d;
  return out;
}
