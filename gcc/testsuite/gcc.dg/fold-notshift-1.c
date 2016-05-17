/* PR tree-optimization/54579
   PR middle-end/55299 */

/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

int
asr1 (int a, int b)
{
  return ~((~a) >> b);
}

long
asr1l (long a, long b)
{
  return ~((~a) >> b);
}

int
asr_conv (unsigned a, unsigned b)
{
  return ~((int)~a >> b);
}

unsigned
asr_conv2 (unsigned a, unsigned b)
{
  return ~(unsigned)((int)~a >> b);
}

unsigned
asr_conv3 (int a, int b)
{
  return ~(unsigned)(~a >> b);
}

typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;

int32_t
asr_conv4 (int64_t a, int b)
{
  return ~((int32_t)~a >> b);
}

int32_t
asr_conv5 (int64_t a, int b)
{
  return ~(int32_t)(~a >> b);
}

int
asr2 (int a, int b)
{
  return -((-a - 1) >> b) - 1;
}

int
asr3 (int a, int b)
{
  return a < 0 ? ~((~a) >> b) : a >> b;
}

int64_t
asr3l (int64_t a, int b)
{
  return a < 0 ? ~((~a) >> b) : a >> b;
}

int
asr4 (int a, int b)
{
  return a < 0 ? -((-a - 1) >> b) - 1 : a >> b;
}

/* { dg-final { scan-tree-dump-times ">>" 11 "cddce1" } } */
/* { dg-final { scan-tree-dump-not "~" "cddce1" } } */
