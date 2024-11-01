#ifndef FP8_HELPER_UNCLUDED
#define FP8_HELPER_UNCLUDED

#include <stdint.h>
#include <inttypes.h>

typedef union 
{
  _Float16 f16;
  unsigned short u16;
} Float16Union;

typedef union
{
  float f;
  uint32_t u;
} Floatuint32Union;

static unsigned char
convert_fp16_to_hf8 (_Float16 x, unsigned char b, int s)
{
  Float16Union ux = { .f16 = x };
  const unsigned short fp16_bias = 15, hf8_bias = 7;
  unsigned short sign = (ux.u16 & 0x8000) >> 8;
  unsigned short e_fp16 = (ux.u16 & 0x7c00) >> 10;
  unsigned short m_fp16 = ux.u16 & 0x03ff;

  /* If bias */
  unsigned short x_bias = b ? ux.u16 + (b >> 1) : ux.u16;
  unsigned short e = (x_bias & 0x7c00) >> 10;
  unsigned short m = (x_bias & 0x03ff) >> 7;

  if (e_fp16 == 0x1f)
  {
    /* Special value: NaN or Infinity. */
    return (0xf << 3) | 0x7 | sign;
  }
  else if ((e_fp16 > (fp16_bias - hf8_bias + 15))
          || ((e_fp16 == (fp16_bias - hf8_bias + 15))
          && (m_fp16 > 0x0300)))
  {
    /* Overflow: Return Max or NaN. */
    return (0xf << 3) | (s ? 0x6 : 0x7) | sign;
  }
  else if (e_fp16 < fp16_bias - hf8_bias - 3)
  {
    /* Value too small: Return zero. */
    return sign;
  }
  else if (e_fp16 <= fp16_bias - hf8_bias)
  {
    /* Denormalized value: Adjust mantissa. */
    m = ((m_fp16 | 0x0400) >> ((fp16_bias - hf8_bias) + 1 - e_fp16))
        | (((m_fp16 & 0x007f) + 0x007f) >> 7);
    return sign;
  }
  else
  {
    /* Normal value: Adjust exponent and mantissa. */
    e -= (fp16_bias - hf8_bias);
    return (e << 3) | m | sign;
  }
}

static unsigned char
convert_fp16_to_bf8 (_Float16 x, unsigned char b, int s)
{
  Float16Union ux = { .f16 = x };
  unsigned short temp;
  unsigned short fp8_res = 0;

  if (__builtin_isinf (x) || __builtin_isnan (x))
  {
    /* Special value: NaN or Infinity. */
    fp8_res = (ux.u16 >> 8) & 0xFF;
    if (__builtin_isnan (x))
      fp8_res |= 0x02;
  }
  else
  {
    unsigned short rounding_bias = b ? b & 0xFF 
                                     : ((ux.u16 >> 8) & 0x1) + 0x7F;
    temp = ux.u16 + rounding_bias;
    fp8_res = (temp >> 8) & 0xFF;
    if (((temp >> 8) & 0x7F) == 0x7C && s)
      fp8_res = (fp8_res & 0x80) | 0x7B;
    }
  return fp8_res;
}

static unsigned char
convert_fp16_to_fp8 (_Float16 x, unsigned char b, int y, int s)
{
  return y ? convert_fp16_to_bf8 (x, b, s) 
           : convert_fp16_to_hf8 (x, b, s);
}

static _Float16
convert_bf8_to_fp16(unsigned char x)
{
  Float16Union u = {.u16 = (x << 8) & 0xff00};
  return u.f16;
}

static _Float16
convert_hf8_to_fp16(unsigned char x)
{
  unsigned char hf8_bias;
  Float16Union res;
  unsigned short fp_16bias, s, e, m, e_norm, lz_cnt;

  fp_16bias = 15;
  hf8_bias = 7;
  s = (x & 0x80) << 8;
  e = (x & 0x78) >> 3;
  m = x & 0x07;
  e_norm = e + fp_16bias - hf8_bias;

  /* convert denormal hf8 number into a normal fp16 number */
  if ((e == 0) && (m !=0))
  {
    lz_cnt = 2;
    lz_cnt = (m > 0x1) ? 1 : lz_cnt;
    lz_cnt = (m > 0x3) ? 0 : lz_cnt;
    e_norm -= lz_cnt;
    m = (m << (lz_cnt + 1)) & 0x07;
  }
  else if ((e == 0) && (m == 0))
    e_norm = 0;
  else if ((e == 0xf) && (m == 0x7))
  {
    e_norm = 0x1f;
    m = 0x4;
  }

  res.u16 = 0;
  res.u16 |= e_norm << 10;
  res.u16 |= m << 7;
  res.u16 |= s;

  return res.f16;
}

static float
convert_bf8_to_fp32 (unsigned char x)
{
  Float16Union u = {.u16 = (x << 8) & 0xff00};
  return (float)(u.f16);
}

static float
convert_hf8_to_fp32 (unsigned char x)
{
  Floatuint32Union res = {.f = 0.0f};
  unsigned int s = (x & 0x80) << 24, e = (x & 0x78) >> 3, m = x & 0x07;
  unsigned int e_norm = e + 120;

  if (!e && m)
    {
      unsigned int lz_cnt = 2 - (m > 1) - (m > 3);
      e_norm -= lz_cnt;
      m = (m << (lz_cnt + 1) & 0x7);
    }
  else if (!e)
    e_norm = 0;
  else if (e == 0xf && m == 0x7)
    e_norm = 255, m = 4;

  res.u |= (e_norm << 23) | (m << 20) | s;

  return res.f;
}

static float
convert_fp8_to_fp32 (unsigned char x, int y)
{
  return y ? convert_bf8_to_fp32 (x) 
           : convert_hf8_to_fp32 (x);
}

static int
fp8_isNan(unsigned char value, int bf8) 
{
  unsigned char e = bf8 ? (value >> 2) & 0b11111 : (value >> 3) & 0b1111;
  unsigned char m = bf8 ? value & 0b11 : value & 0b111;

  return bf8 ? (e == 0b11111 && m != 0) : (e == 0b1111 && m == 0b111);
}

static int
bf8_isInf(unsigned char value) 
{
  unsigned char sign = value >> 7;
  unsigned char e = (value >> 2) & 0b11111;
  unsigned char m = value & 0b11;
    
  if (e == 0b11111 && !m)
    return sign ? -1 : 1;
  return 0;
}

#ifdef __x86_64__
/* type = 0 for hf8 and type = 1 for bf8  */
/* value is 2^16*x for bf8 and 2^9*x for hf8 */
/* +-3 means +-inf, 
   +-2 means +-Nan, 
   and we will use -2 only, 
   +-1 means positive and negative no  rmal numbers, 
   0 means undefined/not initialised */
static int64_t
shift_fp8_to_int64 (unsigned char x, int bf8, int *valueState)
{
  *valueState = (x >> 7) ? -1 : 1;
  if (fp8_isNan (x, bf8))
    *valueState = -2;
  if (bf8)
    if (bf8_isInf(x) == 1)
      *valueState = 3;
    else if (bf8_isInf(x) == -1)
      *valueState = -3;

  unsigned short sign = (x & 0x80) >> 7;
  unsigned short exp = bf8 ? (x & 0x7c) >> 2 : (x & 0x78) >> 3;
  unsigned short frac = bf8 ? (x & 0x03) : (x & 0x07);
  unsigned short mant = (exp == 0) ? frac : (bf8 ? (frac | 0x4) : (frac | 0x8));
  unsigned short e_count = (exp == 0) ? 0 : exp - 1;
  int64_t magnitude = (int64_t)mant << (int64_t)e_count;

  return sign ? -magnitude : magnitude;
}


/* type = 0 for hf8 and type = 1 for bf8  */
static float
shift_int128_to_fp32 (__int128_t in, int type1, int type2)
{
  if (in == 0)
    return 0;
  
  unsigned short sign = (in >> 127) & 1;
  unsigned short Jbit_position = 126;
  unsigned short fac = (type1 == type2) ? ( (type1) ? 32 : 18) : 25;

  __int128_t magnitude = sign ? -in : in;

  while (((magnitude >> 126) & 1) == 0)
    {
	Jbit_position --;
	magnitude <<= 1;
    }

  __int128_t sticky = (magnitude & (((__int128_t)1 << 102) - 1)) != 0;
  __int128_t Gbit = (magnitude >> 102) & 1;
  __int128_t Lbit = (magnitude >> 103) & 1;
  __int128_t RndAddl = Gbit & ( Lbit | sticky);
  __int128_t mant = (magnitude >> 103) & (((__int128_t)1 << 25) - 1);

  __int128_t Rndmant = mant + RndAddl;
  __int128_t Ovf = Rndmant >> 24;
  __int128_t exp = 127 + Jbit_position - fac + Ovf;
  __int128_t frac = Rndmant & 0x7FFFFF;

  Floatuint32Union res;
  res.u = sign << 31;
  res.u |= exp << 23;
  res.u |= frac;

  return res.f;
}
#endif

#endif
