#include "sse4_2-check.h"

#include <nmmintrin.h>
#include <string.h>

#define POLYNOMIAL 0x11EDC6F41LL

#define MAX_BUF 16

static void
shift_mem_by1 (unsigned char* buf, int len)
{
  int i;

  for (i = len - 1; i >= 0; i--)
    {
      buf[i] = buf[i] << 1;
      if (i > 0 && (buf[i-1] & 0x80))
	buf[i] |= 1;
   }
}

static void
do_div (unsigned char* buf, unsigned char* div) 
{
  int i;
  for (i = 0; i < 5; i++)
    buf[i] ^= div[i];
}

static unsigned int
calc_rem (unsigned char* buf, int len)
{
  union
    {
      unsigned long long ll;
      unsigned char c[8];
    } divisor;
  union
    {
      unsigned int i;
      unsigned char c[4];
    } ret;
  unsigned char *div_buf;
  unsigned char divident[MAX_BUF];
  int disp = len / 8;
  int i;

  divisor.ll = POLYNOMIAL << 7LL;

  memcpy (divident, buf, disp);

  div_buf = divident + disp - 5;

  for (i = 0; i < len - 32; i++)
    {
      if ((div_buf[4] & 0x80)) 
	do_div (div_buf, divisor.c);
      shift_mem_by1 (divident, disp);
   }

  memcpy (ret.c, div_buf + 1, sizeof (ret));
  return ret.i;
}

static void 
reverse_bits (unsigned char *src, int len)
{
  unsigned char buf[MAX_BUF];
  unsigned char *tmp = buf + len - 1;
  unsigned char ch;
  int i, j;

  for (i = 0; i < len; i++)
    {
      ch = 0;
      for (j = 0; j < 8; j++)
	if ((src[i] & (1 << j)))
	  ch |= 1 << (7 - j);
      *tmp-- = ch;
    }

  for (i = 0; i < len; i++)
    src[i] = buf[i];
}

static void 
shift_mem ( unsigned char *src, unsigned char *dst, int len, int shft)
{
  int disp = shft / 8;
  int i;
   
  memset (dst, 0, len + disp);
  for (i = 0; i < len; i++)
    dst[i + disp] = src[i];
}

static void 
xor_mem (unsigned char *src, unsigned char *dst, int len)
{
  int disp = len / 8;
  int i;
   
  for (i = 0; i < disp; i++)
    dst[i] ^= src[i];
}

static DST_T
compute_crc32 (DST_T crc, SRC_T inp)
{
  unsigned char crcbuf[sizeof (DST_T)];
  unsigned char inbuf[sizeof (SRC_T)];
  unsigned char tmp1[MAX_BUF], tmp2[MAX_BUF];
  int crc_sh, xor_sz;
  union
    {
      unsigned int i;
      unsigned char c[4];
    } ret;

  crc_sh = sizeof (SRC_T) * 8;
  xor_sz = 32 + crc_sh;
  memcpy (crcbuf, &crc, sizeof (DST_T));
  memcpy (inbuf, &inp, sizeof (SRC_T));
   
  reverse_bits (crcbuf, 4);
  reverse_bits (inbuf, sizeof (SRC_T));

  shift_mem (inbuf, tmp1, sizeof (SRC_T), 32);
  shift_mem (crcbuf, tmp2, 4, crc_sh);

  xor_mem (tmp1, tmp2, xor_sz);

  ret.i = calc_rem (tmp2, xor_sz);
  
  reverse_bits (ret.c, 4);

  return (DST_T)ret.i;
}

#define NUM 1024

static void
sse4_2_test (void)
{
  DST_T dst[NUM]; 
  SRC_T src[NUM];
  int i;

 for (i = 0; i < NUM; i++)
   {
     dst[i] = rand ();
     if (sizeof (DST_T) > 4)
       dst[i] |= (DST_T)rand () << (DST_T)(sizeof (DST_T) * 4);
     src[i] = rand ();
     if (sizeof (SRC_T) > 4)
       src[i] |= (SRC_T)rand () << (SRC_T)(sizeof (DST_T) * 4);
   }

  for (i = 0; i < NUM; i++)
    if (CRC32 (dst[i], src[i]) != compute_crc32 (dst[i], src[i]))
      abort ();
}
