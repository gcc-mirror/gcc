/* { dg-do compile } */

typedef signed char int8_t;
typedef signed long long int64_t;
int64_t
f0a (int8_t * __restrict__ arg1)
{
  int idx;
  int64_t result = 0;
  for (idx = 0; idx < 416; idx += 1)
    result += arg1[idx] << (arg1[idx] == arg1[idx]);
  return result;
}

