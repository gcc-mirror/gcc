/* { dg-require-effective-target untyped_assembly } */
/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

foo (a, b, c, d, e, f, g, h, i)
{
  return foo () + i;
}
