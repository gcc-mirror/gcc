/* { dg-do compile } */
/* { dg-options "-O2 -mprefer-vector-width=512 -fdump-tree-optimized" } */

/* { dg-final { scan-tree-dump-times "\.REDUC_PLUS" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.REDUC_MIN" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\.REDUC_MAX" 3 "optimized" } } */

_Float16
__attribute__((noipa, target("avx512fp16,avx512vl"), optimize("Ofast")))
reduc_add_128 (_Float16* p)
{
  _Float16 sum = 0;
  for (int i = 0; i != 8; i++)
    sum += p[i];
  return sum;
}

_Float16
__attribute__((noipa, target("avx512fp16,avx512vl"), optimize("Ofast")))
reduc_add_256 (_Float16* p)
{
  _Float16 sum = 0;
  for (int i = 0; i != 16; i++)
    sum += p[i];
  return sum;
}

_Float16
__attribute__((noipa, target("avx512fp16,avx512vl"), optimize("Ofast")))
reduc_add_512 (_Float16* p)
{
  _Float16 sum = 0;
  for (int i = 0; i != 32; i++)
    sum += p[i];
  return sum;
}

_Float16
__attribute__((noipa, target("avx512fp16,avx512vl"), optimize("Ofast")))
reduc_min_128 (_Float16* p)
{
  _Float16 sum = p[0];
  for (int i = 0; i != 8; i++)
    sum = sum > p[i] ? p[i] : sum;
  return sum;
}

_Float16
__attribute__((noipa, target("avx512fp16,avx512vl"), optimize("Ofast")))
reduc_min_256 (_Float16* p)
{
  _Float16 sum = p[0];
  for (int i = 0; i != 16; i++)
    sum = sum > p[i] ? p[i] : sum;
  return sum;
}

_Float16
__attribute__((noipa, target("avx512fp16,avx512vl"), optimize("Ofast")))
reduc_min_512 (_Float16* p)
{
  _Float16 sum = p[0];
  for (int i = 0; i != 32; i++)
    sum = sum > p[i] ? p[i] : sum;
  return sum;
}

_Float16
__attribute__((noipa, target("avx512fp16,avx512vl"), optimize("Ofast")))
reduc_max_128 (_Float16* p)
{
  _Float16 sum = p[0];
  for (int i = 0; i != 8; i++)
    sum = sum < p[i] ? p[i] : sum;
  return sum;
}

_Float16
__attribute__((noipa, target("avx512fp16,avx512vl"), optimize("Ofast")))
reduc_max_256 (_Float16* p)
{
  _Float16 sum = p[0];
  for (int i = 0; i != 16; i++)
    sum = sum < p[i] ? p[i] : sum;
  return sum;
}

_Float16
__attribute__((noipa, target("avx512fp16,avx512vl"), optimize("Ofast")))
reduc_max_512 (_Float16* p)
{
  _Float16 sum = p[0];
  for (int i = 0; i != 32; i++)
    sum = sum < p[i] ? p[i] : sum;
  return sum;
}
