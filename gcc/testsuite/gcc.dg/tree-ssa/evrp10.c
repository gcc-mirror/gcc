/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" }*/

typedef __INT32_TYPE__ int32_t;

int32_t and(int32_t x, int32_t y)
{
  int32_t tx = x >> 24;
  int32_t ty = y >> 24;
  int32_t t = tx & ty;
  return t;
}

int32_t ior(int32_t x, int32_t y)
{
  int32_t tx = x >> 24;
  int32_t ty = y >> 24;
  int32_t t = tx | ty;
  return t;
}

int32_t xor(int32_t x, int32_t y)
{
  int32_t tx = x >> 24;
  int32_t ty = y >> 24;
  int32_t t = tx ^ ty;
  return t;
}

/* { dg-final { scan-tree-dump-times "\\\[-128, 127\\\]" 9 "evrp" } } */
