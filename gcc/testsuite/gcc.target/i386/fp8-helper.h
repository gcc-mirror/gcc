#ifndef FP8_HELPER_UNCLUDED
#define FP8_HELPER_UNCLUDED

typedef union 
{
  _Float16 f16;
  unsigned short u16;
} Float16Union;

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

#endif
