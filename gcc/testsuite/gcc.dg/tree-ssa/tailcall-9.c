/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-tailc-details" } */

__attribute__((noinline))
static float f(float x)
{
  return x*x;
}

static double g(float x)
{
  return x>0 ? f(x) : x+1.0;
}

float foo(float x)
{
  return g(x);
}

/* { dg-final { scan-tree-dump "Found tail call" "tailc" } } */
