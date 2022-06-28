/* { dg-do run } */

__INT32_TYPE__ a = 5, b, c, d;
__UINT64_TYPE__ e = 20862985922;
int main()
{
  __UINT32_TYPE__ f = 4294967292;
  e = e | f;
  c = -1 % ((~f ^ 4294967292) - (e - d));
  b = ~-~e % ~-d;
  if (b)
    a = 0;
  if (a < 1)
    __builtin_abort();
  return 0;
}
