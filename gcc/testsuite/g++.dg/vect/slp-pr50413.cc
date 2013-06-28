/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

typedef unsigned long long UInt64;

typedef struct struct128
{
  UInt64 uint64_lower;
  UInt64 uint64_upper;
}
STRUCT_128;

typedef union uint128_bitmap
{
  STRUCT_128 uint128;
  
    struct
    {
      UInt64 b00 : 1;
      UInt64 b01 : 1;
      UInt64 b02 : 1;
      UInt64 b03 : 1;
      UInt64 b04 : 1;
      UInt64 b05 : 1;
      UInt64 b06 : 1;
      UInt64 b07 : 1;
      UInt64 b08 : 1;
      UInt64 b09 : 1;
      UInt64 b10 : 1;
      UInt64 b11 : 1;
      UInt64 b12 : 1;
      UInt64 b13 : 1;
      UInt64 b14 : 1;
      UInt64 b15 : 1;
      UInt64 b16 : 1;
      UInt64 b17 : 1;
      UInt64 b18 : 1;
      UInt64 b19 : 1;
      UInt64 b20 : 1;
      UInt64 b21 : 1;
      UInt64 b22 : 1;
      UInt64 b23 : 1;
      UInt64 b24 : 1;
      UInt64 b25 : 1;
      UInt64 b26 : 1;
      UInt64 b27 : 1;
      UInt64 b28 : 1;
      UInt64 b29 : 1;
      UInt64 b30 : 1;
      UInt64 b31 : 1;
      UInt64 b32 : 1;
      UInt64 b33 : 1;
      UInt64 b34 : 1;
      UInt64 b35 : 1;
      UInt64 b36 : 1;
      UInt64 b37 : 1;
      UInt64 b38 : 1;
      UInt64 b39 : 1;
      UInt64 b40 : 1;
      UInt64 b41 : 1;
      UInt64 b42 : 1;
      UInt64 b43 : 1;
      UInt64 b44 : 1;
      UInt64 b45 : 1;
      UInt64 b46 : 1;
      UInt64 b47 : 1;
      UInt64 b48 : 1;
      UInt64 b49 : 1;
      UInt64 b50 : 1;
      UInt64 b51 : 1;
      UInt64 b52 : 1;
      UInt64 b53 : 1;
      UInt64 b54 : 1;
      UInt64 b55 : 1;
      UInt64 b56 : 1;
      UInt64 b57 : 1;
      UInt64 b58 : 1;
      UInt64 b59 : 1;
      UInt64 b60 : 1;
      UInt64 b61 : 1;
      UInt64 b62 : 1;
      UInt64 b63 : 1;
      UInt64 b64 : 1;
      UInt64 b65 : 1;
      UInt64 b66 : 1;
      UInt64 b67 : 1;
      UInt64 b68 : 1;
      UInt64 b69 : 1;
      UInt64 b70 : 1;
      UInt64 b71 : 1;
      UInt64 b72 : 1;
      UInt64 b73 : 1;
      UInt64 b74 : 1;
      UInt64 b75 : 1;
      UInt64 b76 : 1;
      UInt64 b77 : 1;
      UInt64 b78 : 1;
      UInt64 b79 : 1;
      UInt64 b80 : 1;
      UInt64 b81 : 1;
      UInt64 b82 : 1;
      UInt64 b83 : 1;
      UInt64 b84 : 1;
      UInt64 b85 : 1;
      UInt64 b86 : 1;
      UInt64 b87 : 1;
      UInt64 b88 : 1;
      UInt64 b89 : 1;
      UInt64 b90 : 1;
      UInt64 b91 : 1;
      UInt64 b92 : 1;
      UInt64 b93 : 1;
      UInt64 b94 : 1;
      UInt64 b95 : 1;
      UInt64 b96 : 1;
      UInt64 b97 : 1;
      UInt64 b98 : 1;
      UInt64 b99 : 1;
      UInt64 b100 : 1;
      UInt64 b101 : 1;
      UInt64 b102 : 1;
      UInt64 b103 : 1;
      UInt64 b104 : 1;
      UInt64 b105 : 1;
      UInt64 b106 : 1;
      UInt64 b107 : 1;
      UInt64 b108 : 1;
      UInt64 b109 : 1;
      UInt64 b110 : 1;
      UInt64 b111 : 1;
      UInt64 b112 : 1;
      UInt64 b113 : 1;
      UInt64 b114 : 1;
      UInt64 b115 : 1;
      UInt64 b116 : 1;
      UInt64 b117 : 1;
      UInt64 b118 : 1;
      UInt64 b119 : 1;
      UInt64 b120 : 1;
      UInt64 b121 : 1;
      UInt64 b122 : 1;
      UInt64 b123 : 1;
      UInt64 b124 : 1;
      UInt64 b125 : 1;
      UInt64 b126 : 1;
      UInt64 b127 : 1;
    }
    bitmap;
}
UInt128_BITMAP;

UInt128_BITMAP V;

void shift(unsigned char t)
{
  V.uint128.uint64_lower = (V.uint128.uint64_lower >> 1);
  V.bitmap.b63 = V.bitmap.b64;
  V.uint128.uint64_upper = (V.uint128.uint64_upper >> 1);
  
  V.bitmap.b96 = t;
}

/* { dg-final { scan-tree-dump-times "Vectorized basic-block" 0 "slp" } } */
/* { dg-final { cleanup-tree-dump "slp" } } */

