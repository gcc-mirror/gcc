/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-fdump-tree-gimple" } */

/* Checks the simplification of:
   1 << (C - x) to (1 << C) >> x, where C = precision (type) - 1
   f1 is not simplified but f2, f3 and f4 are. */

__INT64_TYPE__ f1 (__INT64_TYPE__ i)
{
  return (__INT64_TYPE__)1 << (31 - i);
}

__INT64_TYPE__ f2 (__INT64_TYPE__ i)
{
  return (__INT64_TYPE__)1 << (63 - i);
}

__UINT64_TYPE__ f3 (__INT64_TYPE__ i)
{
  return (__UINT64_TYPE__)1 << (63 - i);
}

__INT32_TYPE__ f4 (__INT32_TYPE__ i)
{
  return (__INT32_TYPE__)1 << (31 - i);
}

/* { dg-final { scan-tree-dump-times "= 31 -"  1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "9223372036854775808 >>" 2 "gimple" } } */
/* { dg-final { scan-tree-dump "2147483648 >>" "gimple" } } */
