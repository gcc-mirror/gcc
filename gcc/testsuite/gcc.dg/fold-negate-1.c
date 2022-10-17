/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#define SHIFT ((8*__SIZEOF_INT__)-1)

int test_rshift_1(int x)
{
  int t = x >> SHIFT;
  return -t;
}

int test_rshift_2(int x)
{
  unsigned int t = (unsigned int)x >> SHIFT;
  return -t;
}

int test_rshift_3(int x)
{
  int t = (unsigned int)x >> SHIFT;
  return -t;
}

int test_rshift_4(int x)
{
  unsigned int t = x >> SHIFT;
  return -t;
}

double test_mul_1(double x)
{
  double t = -5.0 * x;
  return -t;
}

double test_mul_2(double x, double y)
{
  double t1 = -x;
  double t2 = t1 * y;
  return -t2;
}

double test_rdiv_1(double x, double y)
{
  double t1 = -x;
  double t2 = t1 / y;
  return -t2;
}

double test_rdiv_2(double x, double y)
{
  double t1 = -y;
  double t2 = x / t1;
  return -t2;
}

/* { dg-final { scan-tree-dump-not " -" "optimized" } } */

