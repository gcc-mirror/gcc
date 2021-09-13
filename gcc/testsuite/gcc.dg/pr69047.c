/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop4" } */

__UINT8_TYPE__
f(__UINT16_TYPE__ b)
{
  __UINT8_TYPE__ a;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  __builtin_memcpy(&a, &b, sizeof a);
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  __builtin_memcpy(&a, (char *)&b + sizeof a, sizeof a);
#else
  a = b;
#endif
  return a;
}

/* { dg-final { scan-tree-dump "_\[0-9\]+ = \\(\[^)\]+\\) b" "forwprop4" } } */
