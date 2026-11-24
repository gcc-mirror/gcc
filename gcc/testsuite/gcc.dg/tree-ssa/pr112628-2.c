/* PR tree-optimization/112628 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* For signed A (overflow undefined), ~A < -A is always true.
   For any integer type, ~A == -A is always false.  */

int
f (int a)
{
  return (~a) < -a;
}

int
f0 (int a)
{
  return a > ~(-a);
}

int
fne (int a)
{
  return (~a) != -a;
}

int
feq (int a)
{
  return (~a) == -a;
}

int
ufeq (unsigned a)
{
  return (~a) == -a;
}

/* { dg-final { scan-tree-dump-times "return 1;" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 0;" 2 "optimized" } } */
