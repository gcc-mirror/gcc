/* { dg-require-effective-target untyped_assembly } */
foo (a, b, c, d, e, f, g, h, i)
{
  return foo () + i;
}
