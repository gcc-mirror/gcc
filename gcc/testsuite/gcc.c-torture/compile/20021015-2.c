/* PR target/8232.  */
/* { dg-require-effective-target untyped_assembly } */

int f (char *p, char *q, int i)
{
  return __builtin_bcmp (p, q, i);
}

