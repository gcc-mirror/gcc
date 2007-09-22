/* { dg-do run } */
/* { dg-require-effective-target sse5 } */
/* { dg-options "-O2 -msse5" } */

#include "sse5-check.h"

#include <bmmintrin.h>
#include <string.h>

#define NUM 10

union
{
  __m128i x[NUM];
  int8_t ssi[NUM * 16];
  int16_t si[NUM * 8];
  int32_t li[NUM * 4];
  int64_t lli[NUM * 2];
} dst, res, src1;

static void
init_sbyte ()
{
  int i;
  for (i=0; i < NUM * 16; i++)
    src1.ssi[i] = i;
}

static void
init_sword ()
{
  int i;
  for (i=0; i < NUM * 8; i++)
    src1.si[i] = i;
}


static void
init_sdword ()
{
  int i;
  for (i=0; i < NUM * 4; i++)
    src1.li[i] = i;
}

static int 
check_sbyte2word ()
{
  int i, j, s, t, check_fails = 0;
  for (i = 0; i < NUM * 16; i = i + 16)
    {
      for (j = 0; j < 8; j++)
	{
	  t = i + (2 * j);
	  s = (i / 2) + j;
	  res.si[s] = src1.ssi[t] - src1.ssi[t + 1] ;
	  if (res.si[s] != dst.si[s]) 
	    check_fails++;	
	}
    }
}

static int
check_sword2dword ()
{
  int i, j, s, t, check_fails = 0;
  for (i = 0; i < (NUM * 8); i = i + 8)
    {
      for (j = 0; j < 4; j++)
	{
	  t = i + (2 * j);
	  s = (i / 2) + j;
	  res.li[s] = src1.si[t] - src1.si[t + 1] ;
	  if (res.li[s] != dst.li[s]) 
	    check_fails++;	
	}
    }
}

static int
check_dword2qword ()
{
  int i, j, s, t, check_fails = 0;
  for (i = 0; i < (NUM * 4); i = i + 4)
    {
      for (j = 0; j < 2; j++)
	{
	  t = i + (2 * j);
	  s = (i / 2) + j;
	  res.lli[s] = src1.li[t] - src1.li[t + 1] ;
	  if (res.lli[s] != dst.lli[s]) 
	    check_fails++;	
	}
    }
}

static void
sse5_test (void)
{
  int i;
  
  /* Check hsubbw */
  init_sbyte ();
  
  for (i = 0; i < NUM; i++)
    dst.x[i] = _mm_hsubw_epi8 (src1.x[i]);
  
  if (check_sbyte2word())
  abort ();
  

  /* Check hsubwd */
  init_sword ();

  for (i = 0; i < (NUM ); i++)
    dst.x[i] = _mm_hsubd_epi16 (src1.x[i]);
  
  if (check_sword2dword())
    abort (); 
   
   /* Check hsubdq */
  init_sdword ();
    for (i = 0; i < NUM; i++)
    dst.x[i] = _mm_hsubq_epi32 (src1.x[i]);
  
  if (check_dword2qword())
    abort ();
}
