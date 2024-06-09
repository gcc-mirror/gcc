/* { dg-require-effective-target untyped_assembly } */
/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */
/* { dg-additional-options "-std=gnu89" } */

int
foo (a, b, c)
{
  return a + b + c;
}

int
bar ()
{
  int q, w, e, r, t, y;

  return foo ((int) & q, q, w, e, q, (int) &w);
}
