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
  unsigned char  ssi[NUM * 16];
  unsigned short si[NUM * 8];
  unsigned int li[NUM * 4];
  unsigned long long  lli[NUM * 2];
} dst, res, src1;

static void
init_byte ()
{
  int i;
  for (i=0; i < NUM * 16; i++)
    src1.ssi[i] = i;
}

static void
init_word ()
{
  int i;
  for (i=0; i < NUM * 8; i++)
    src1.si[i] = i;
}


static void
init_dword ()
{
  int i;
  for (i=0; i < NUM * 4; i++)
    src1.li[i] = i;
}

static int 
check_byte2word ()
{
  int i, j, s, t, check_fails = 0;
  for (i = 0; i < NUM * 16; i = i + 16)
    {
      for (j = 0; j < 8; j++)
	{
	  t = i + (2 * j);
	  s = (i / 2) + j;
	  res.si[s] = src1.ssi[t] + src1.ssi[t + 1] ;
	  if (res.si[s] != dst.si[s]) 
	    check_fails++;	
	}
    }
}

static int 
check_byte2dword ()
{
  int i, j, s, t, check_fails = 0;
  for (i = 0; i < NUM * 16; i = i + 16)
    {
      for (j = 0; j < 4; j++)
	{
	  t = i + (4 * j);
	  s = (i / 4) + j;
	  res.li[s] = (src1.ssi[t] + src1.ssi[t + 1]) + (src1.ssi[t + 2]
	              + src1.ssi[t + 3]); 
	  if (res.li[s] != dst.li[s]) 
	    check_fails++;
	}
    }
  return check_fails++;
}

static int
check_byte2qword ()
{
  int i, j, s, t, check_fails = 0;
  for (i = 0; i < NUM * 16; i = i + 16)
    {
      for (j = 0; j < 2; j++)
	{
	  t = i + (8 * j);
	  s = (i / 8) + j;
	  res.lli[s] = ((src1.ssi[t] + src1.ssi[t + 1]) + (src1.ssi[t + 2] 
		       + src1.ssi[t + 3])) + ((src1.ssi[t + 4] + src1.ssi[t +5])
	               + (src1.ssi[t + 6] + src1.ssi[t + 7])); 
	  if (res.lli[s] != dst.lli[s]) 
	    check_fails++;
	}
    }
  return check_fails++;
}

static int
check_word2dword ()
{
  int i, j, s, t, check_fails = 0;
  for (i = 0; i < (NUM * 8); i = i + 8)
    {
      for (j = 0; j < 4; j++)
	{
	  t = i + (2 * j);
	  s = (i / 2) + j;
	  res.li[s] = src1.si[t] + src1.si[t + 1] ;
	  if (res.li[s] != dst.li[s]) 
	    check_fails++;	
	}
    }
}

static int 
check_word2qword ()
{
  int i, j, s, t, check_fails = 0;
  for (i = 0; i < NUM * 8; i = i + 8)
    {
      for (j = 0; j < 2; j++)
	{
	  t = i + (4 * j);
	  s = (i / 4) + j;
	  res.lli[s] = (src1.si[t] + src1.si[t + 1]) + (src1.si[t + 2]
	               + src1.si[t + 3]); 
	  if (res.lli[s] != dst.lli[s]) 
	    check_fails++;
	}
    }
  return check_fails++;
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
	  res.lli[s] = src1.li[t] + src1.li[t + 1] ;
	  if (res.lli[s] != dst.lli[s]) 
	    check_fails++;	
	}
    }
}

static void
sse5_test (void)
{
  int i;
  
  /* Check haddubw */
  init_byte ();
  
  for (i = 0; i < NUM; i++)
    dst.x[i] = _mm_haddw_epu8 (src1.x[i]);
  
  if (check_byte2word())
  abort ();
  
  /* Check haddubd */
  for (i = 0; i < (NUM ); i++)
    dst.x[i] = _mm_haddd_epu8 (src1.x[i]);
  
  if (check_byte2dword())
    abort (); 
  
  /* Check haddubq */
  for (i = 0; i < NUM; i++)
    dst.x[i] = _mm_haddq_epu8 (src1.x[i]);
  
  if (check_byte2qword())
    abort ();

  /* Check hadduwd */
  init_word ();

  for (i = 0; i < (NUM ); i++)
    dst.x[i] = _mm_haddd_epu16 (src1.x[i]);
  
  if (check_word2dword())
    abort (); 
   
  /* Check haddbuwq */
 
  for (i = 0; i < NUM; i++)
    dst.x[i] = _mm_haddq_epu16 (src1.x[i]);
  
  if (check_word2qword())
    abort ();
 
  /* Check hadudq */
  init_dword ();
    for (i = 0; i < NUM; i++)
    dst.x[i] = _mm_haddq_epu32 (src1.x[i]);
  
  if (check_dword2qword())
    abort ();
}
