/* { dg-do run } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

typedef __INT8_TYPE__ v16qi __attribute__((vector_size(16),may_alias,aligned(1)));
typedef __INT32_TYPE__ v4si __attribute__((vector_size(16),may_alias,aligned(1)));

const __INT32_TYPE__ x[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
const __INT8_TYPE__ y[32]
  = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 };
const __INT32_TYPE__ z[8] = { [3] = 3, [1] = 1 };

int
main()
{
  v4si v1 = *(v4si *)(x + 1);
  for (unsigned i = 0; i < 4; ++i)
    if (v1[i] != (v4si) { 2, 3, 4, 5 }[i])
      __builtin_abort ();
  v4si v2 = *(v4si *)z;
  for (unsigned i = 0; i < 4; ++i)
    if (v2[i] != (v4si) { 0, 1, 0, 3 }[i])
      __builtin_abort ();
  v4si v3 = *(v4si *)(y + 1);
  for (unsigned i = 0; i < 4; ++i)
    if (v3[i] !=
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
	(v4si) { 0x01020304, 0x05060708, 0x090a0b0c, 0x0d0e0f10 }[i]
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
	(v4si) { 0x04030201, 0x08070605, 0x0c0b0a09, 0x100f0e0d }[i]
#else
	v3[i]
#endif
       )
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
