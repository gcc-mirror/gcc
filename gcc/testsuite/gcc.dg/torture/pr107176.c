/* { dg-do run } */

__INT32_TYPE__ a;
__INT64_TYPE__ b;
static inline __INT64_TYPE__ c(__UINT32_TYPE__ d)
{
  return d;
}
static inline void e(__INT32_TYPE__ d)
{
  a = d;
}
int main()
{
  b = 0;
  for (; b < 1; b = c(b - 90) + 90 + 1)
    ;
  e(b >> 2);
  if (a != 1073741824)
    __builtin_abort();
  return 0;
}
