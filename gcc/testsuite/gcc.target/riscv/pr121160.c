/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d -ffast-math -O2" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32d -ffast-math -O2" { target { rv32 } } } */


typedef long int ssize_t;
typedef float MagickRealType;
typedef unsigned short Quantum;
typedef unsigned long long MagickSizeType;
typedef struct _PixelPacket
{
  Quantum blue, green, red, opacity;
} PixelPacket;
static inline Quantum
ClampToQuantum (const MagickRealType value)
{
  if (value <= 0.0f)
    return ((Quantum) 0);
  if (value >= (MagickRealType) ((Quantum) 65535))
    return (((Quantum) 65535));
  return ((Quantum) (value + 0.5f));
}

static inline float
HalfToSinglePrecision (const unsigned short half)
{
  typedef union _SinglePrecision
  {
    unsigned int fixed_point;
    float single_precision;
  } SinglePrecision;
  register unsigned int exponent, significand, sign_bit;
  SinglePrecision map;
  unsigned int value;
  if (significand == 0)
    value = sign_bit << 31;
  else
    {
      while ((significand & 0x00000400) == 0)
        {
          significand <<= 1;
        }
      value = (sign_bit << 31) | (exponent << 23) | (significand << 13);
    }
  map.fixed_point = value;
  return (map.single_precision);
}

void
ImportBlueQuantum (const MagickSizeType number_pixels,
                   PixelPacket *restrict q)
{
  register ssize_t x;
  unsigned short pixel;
  {
    for (x = 0; x < (ssize_t) number_pixels; x++)
      q->blue = ClampToQuantum (HalfToSinglePrecision (pixel));
  }
}

