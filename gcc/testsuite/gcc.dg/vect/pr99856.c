/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_unpack } */
/* { dg-require-effective-target vect_pack_trunc } */

#define SHIFTFORDIV255(a)\
    ((((a) >> 8) + a) >> 8)

#define DIV255(a)\
    SHIFTFORDIV255(a + 0x80)

typedef unsigned char uint8_t;

void
opSourceOver_premul(uint8_t* restrict Rrgba,
                    const uint8_t* restrict Srgba,
                    const uint8_t* restrict Drgba, int len)
{
  Rrgba = __builtin_assume_aligned (Rrgba, __BIGGEST_ALIGNMENT__);
  Srgba = __builtin_assume_aligned (Srgba, __BIGGEST_ALIGNMENT__);
  Drgba = __builtin_assume_aligned (Drgba, __BIGGEST_ALIGNMENT__);
  int i = 0;
  for (; i < len*4; i += 4)
    {
      uint8_t Sa = Srgba[i + 3];
      Rrgba[i + 0] = DIV255(Srgba[i + 0] * 255 + Drgba[i + 0] * (255 - Sa));
      Rrgba[i + 1] = DIV255(Srgba[i + 1] * 255 + Drgba[i + 1] * (255 - Sa));
      Rrgba[i + 2] = DIV255(Srgba[i + 2] * 255 + Drgba[i + 2] * (255 - Sa));
      Rrgba[i + 3] = DIV255(Srgba[i + 3] * 255 + Drgba[i + 3] * (255 - Sa));
    }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
