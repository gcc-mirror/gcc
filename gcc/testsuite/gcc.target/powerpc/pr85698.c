/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O3 -mcpu=power7" } */

/* PR85698: Incorrect code generated on LE due to use of stxvw4x. */

typedef unsigned char uint8_t;
typedef short int16_t;
extern void abort (void);
extern int memcmp(const void *, const void *, __SIZE_TYPE__);

uint8_t expected[128] =
{14, 0, 4, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 28, 35, 33, 35, 36, 37, 38, 39, 40,
 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
 60, 61, 62, 63, 66, 63, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,
 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 97, 96,
 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113,
 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127};

static uint8_t x264_clip_uint8( int x )
{
  return x&(~255) ? (-x)>>31 : x;
}
void add4x4_idct( uint8_t *p_dst, int16_t dct[16])
{
  int16_t d[16];
  int16_t tmp[16];
  int i, y, x;
  for( i = 0; i < 4; i++ )
    {
      int s02 =  dct[0*4+i]     +  dct[2*4+i];
      int d02 =  dct[0*4+i]     -  dct[2*4+i];
      int s13 =  dct[1*4+i]     + (dct[3*4+i]>>1);
      int d13 = (dct[1*4+i]>>1) -  dct[3*4+i];
      tmp[i*4+0] = s02 + s13;
      tmp[i*4+1] = d02 + d13;
      tmp[i*4+2] = d02 - d13;
      tmp[i*4+3] = s02 - s13;
    }
  for( i = 0; i < 4; i++ )
    {
      int s02 =  tmp[0*4+i]     +  tmp[2*4+i];
      int d02 =  tmp[0*4+i]     -  tmp[2*4+i];
      int s13 =  tmp[1*4+i]     + (tmp[3*4+i]>>1);
      int d13 = (tmp[1*4+i]>>1) -  tmp[3*4+i];
      d[0*4+i] = ( s02 + s13 + 32 ) >> 6;
      d[1*4+i] = ( d02 + d13 + 32 ) >> 6;
      d[2*4+i] = ( d02 - d13 + 32 ) >> 6;
      d[3*4+i] = ( s02 - s13 + 32 ) >> 6;
    }
  for( y = 0; y < 4; y++ )
    {
      for( x = 0; x < 4; x++ )
        p_dst[x] = x264_clip_uint8( p_dst[x] + d[y*4+x] );
      p_dst += 32;
    }
}

int main()
{
  uint8_t dst[128];
  int16_t dct[16];
  int i;

  for (i = 0; i < 16; i++)
    dct[i] = i*10 + i;
  for (i = 0; i < 128; i++)
    dst[i] = i;

  add4x4_idct(dst, dct);

  if (memcmp (dst, expected, 128))
    abort();

 return 0;
}

