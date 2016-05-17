/* PR middle-end/55299 */

/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

unsigned int
lsr (unsigned int a, unsigned int b)
{
  return ~((~a) >> b);
}

int
sl (int a, int b)
{
  return ~((~a) << b);
}

typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;

int64_t
asr_widen1 (int32_t a, int b)
{
  return ~((int64_t)(~a) >> b);
}

int64_t
asr_widen2 (int32_t a, int b)
{
  return ~(int64_t)(~a >> b);
}

/* { dg-final { scan-tree-dump-times "~" 8 "cddce1" } } */
