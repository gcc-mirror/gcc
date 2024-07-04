/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9 -O2 -save-temps" } */
/* { dg-final { scan-assembler-times {\mbcdadd\M} 5 } } */
/* { dg-final { scan-assembler-times {\mbcdsub\M} 20 } } */
/* { dg-final { scan-assembler-times {\mbcds\M} 2 } } */
/* { dg-final { scan-assembler-times {\mdenbcdq\M} 1 } } */

#include <altivec.h>

#define DEBUG 0

#if DEBUG
#include <stdio.h>
#endif


#define BCD_POS0  12    //  0xC
#define BCD_POS1  15    //  0xF
#define BCD_NEG   13    //  0xD

void abort (void);

  union conv_t
    {
      _Decimal128 d128;
      vector  unsigned char ch;
      vector  long long unsigned int vllui;
    } conv;
  
_Decimal128 convert_vec_char (vector unsigned char a)
{
  union conv_t conv;
  _Decimal128 result;
  
  conv.ch = a;
  result = conv.d128;
  return result;
}
			      
vector unsigned char maxbcd(unsigned int sign)
{
  vector unsigned char result;
  int i;

#ifdef __BIG_ENDIAN__
  for (i = 0; i < 15; i++)
#else
  for (i = 15; i > 0; i--)
#endif
    result[i] = 0x99;

#ifdef __BIG_ENDIAN__
  result[15] = 0x90 | sign;
#else
  result[0] = 0x90 | sign;
#endif

  return result;
}

vector unsigned char num2bcd(long int a, int encoding)
{
  int i;
  unsigned int hi, low, sign;
  
  vector unsigned char result;

  if (a > 0) {
    if (encoding == 0)
      sign = BCD_POS0;
    else
      sign = BCD_POS1;

  } else {
    sign = BCD_NEG;
    a = -a;
  }

  hi = a % 10;   // 1st digit
  a = a / 10;
#ifdef __BIG_ENDIAN__
  result[15] = hi << 4| sign;
#else
  result[0] = hi << 4| sign;
#endif

#ifdef __BIG_ENDIAN__
  for (i = 14; i >= 0; i--)
#else
  for (i = 1; i < 16; i++)
#endif
    {
      low = a % 10;
      a = a / 10;
      hi = a % 10;
      a = a / 10;
      result[i] = hi << 4 | low;
    }


  return result;
}

int main ()
{
  int i;
  long int value_a, value_b, value_result;
  vector unsigned char a, b, result, exp_result;
  _Decimal128 result_d128, exp_result_d128;

  /* Make a and b positive BCD numbers */
  value_a = 1020304;
  a = num2bcd(value_a, 0);

  value_b = 101010;
  b = num2bcd(value_b, 0);
 
  value_result = value_a + value_b;
  exp_result = num2bcd(value_result, 0);
  
  result = __builtin_bcdadd (a, b, 0);

  for (i = 0; i < 16; i++)
    if (exp_result[i] != result[i]) {

#if DEBUG
      printf("ERROR: __builtin_bcdadd result[%d] = %d does not match "
	     "expected_result[%d] = %d\n",
	     i, result[i], i, exp_result[i]);
#else
      abort();
#endif
    }

  /* result should be positive */
#ifdef __BIG_ENDIAN__
  if ((result[15] & 0xF) != BCD_POS0)
#else
  if ((result[0] & 0xF) != BCD_POS0)
#endif
#if DEBUG
      printf("ERROR: __builtin_bcdadd sign of result is %d.  Does not match "
	     "expected_result = %d\n",
	     result[0] & 0xF, BCD_POS0);
#else
      abort();
#endif

  /* Make a and b positive BCD numbers using alternate positive encoding.  */
  value_a = 1030507;
  a = num2bcd(value_a, 1);

  value_b = 204060;
  b = num2bcd(value_b, 1);

  value_result = value_a + value_b;
  exp_result = num2bcd(value_result, 1);
  
  result = __builtin_bcdadd (a, b, 1);

  for (i = 0; i < 16; i++)
    if (exp_result[i] != result[i]) {
#if DEBUG
      printf("ERROR: __builtin_bcdadd result[%d] = %d does not match "
	     "expected_result[%d] = %d\n",
	     i, result[i], i, exp_result[i]);
#else
      abort();
#endif
    }

  /* Result should be positive, alternate encoding.  */
#ifdef __BIG_ENDIAN__
  if ((result[15] & 0xF) != BCD_POS1)
#else
  if ((result[0] & 0xF) != BCD_POS1)
#endif
#if DEBUG
    printf("ERROR: __builtin_bcdadd sign of result is %d.  Does not "
	   "match expected_result = %d\n",
	     result[0] & 0xF, BCD_POS1);
#else
    abort();
#endif

  /* Make a and b negative BCD numbers */
  value_a = -1030507;
  a = num2bcd(value_a, 0);

  value_b = -1010101;
  b = num2bcd(value_b, 0);

  value_result = value_a + value_b;
  exp_result = num2bcd(value_result, 0);
  
  result = __builtin_bcdadd (a, b, 0);

  for (i = 0; i < 16; i++)
    if (exp_result[i]  != result[i]) {
#if DEBUG
      printf("ERROR: __builtin_bcdadd, neg result[%d] = %d does not match "
	     "expected_result[%d] = %d\n",
	     i, result[i], i, exp_result[i]);
#else
      abort();
#endif
    }

  /* result should be negative */
#ifdef __BIG_ENDIAN__
  if ((result[15] & 0xF) != BCD_NEG)
#else
  if ((result[0] & 0xF) != BCD_NEG)
#endif
#if DEBUG
    printf("ERROR: __builtin_bcdadd sign, neg of result is %d.  Does not "
	   "match expected_result = %d\n",
	     result[0] & 0xF, BCD_NEG);
#else
    abort();
#endif

 
  /* Make a negative, b positive BCD numbers */
  value_a = -1030507;
  a = num2bcd(value_a, 0);

  value_b = 1010101;
  b = num2bcd(value_b, 0);

  value_result = value_a - value_b;
  exp_result = num2bcd(value_result, 0);
  
  result = __builtin_bcdsub (a, b, 0);

  for (i = 0; i < 16; i++)
    if (exp_result[i] != result[i]) {
#if DEBUG
      printf("ERROR: __builtin_bcdsub, neg result[%d] = %d does not match "
	     "expected_result[%d] = %d\n",
	     i, result[i], i, exp_result[i]);
#else
      abort();
#endif
    }

  /* result should be positive */
#ifdef __BIG_ENDIAN__
  if ((result[15] & 0xF) != BCD_NEG)
#else
  if ((result[0] & 0xF) != BCD_NEG)
#endif
#if DEBUG
    printf("ERROR: __builtin_bcdadd sign, of result is %d.  Does not match "
	   "expected_result = %d\n",
	     result[0] & 0xF, BCD_NEG);
#else
    abort();
#endif

  /* Make a and b positive BCD numbers */
  value_a = 1030507;
  a = num2bcd(value_a, 1);

  value_b = 1010101;
  b = num2bcd(value_b, 1);

  value_result = value_a - value_b;
  exp_result = num2bcd(value_result, 1);
  
  result = __builtin_bcdsub (a, b, 1);

  for (i = 0; i < 16; i++)
    if (exp_result[i] != result[i]) {
#if DEBUG
      printf("ERROR:carll __builtin_bcdsub, pos result[%d] = %d does not "
	     "match expected_result[%d] = %d\n",
	     i, result[i], i, exp_result[i]);
#else
      abort();
#endif
    }

  /* result should be positive */
#ifdef __BIG_ENDIAN__
  if ((result[15] & 0xF) != BCD_POS1)
#else
  if ((result[0] & 0xF) != BCD_POS1)
#endif
#if DEBUG
    printf("ERROR: __builtin_bcdsub sign, result is %d.  Does not match "
	   "expected_result = %d\n",
	     result[0] & 0xF, BCD_POS1);
#else
      abort();
#endif

  /* Test overflow add and subtract.  */
  a = maxbcd(BCD_POS0);
  b = maxbcd(BCD_POS0);

  if(__builtin_bcdadd_ofl (a, b, 0) == 0)
#if DEBUG
    printf("ERROR: __builtin_bcdadd did not overflow as expected\n");
#else
    abort();
#endif

  value_a = 99999999;
  a = num2bcd(value_a, 0);

  value_b = 999999999;
  b = num2bcd(value_b, 0);

  if(__builtin_bcdadd_ofl (a, b, 0))
#if DEBUG
    printf("ERROR: __builtin_bcdadd unexpectedly overflowed\n");
#else
    abort();
#endif

  a = maxbcd(BCD_POS0);
  b = maxbcd(BCD_NEG);

  if (__builtin_bcdsub_ofl (a, b, 0) == 0)
#if DEBUG
    printf("ERROR: __builtin_bcdsub did not overflow as expected\n");
#else
    abort();
#endif

  value_a = -99999999;
  a = num2bcd(value_a, 0);

  value_b = -999999999;
  b = num2bcd(value_b, 0);

  if (__builtin_bcdsub_ofl (a, b, 0))
#if DEBUG
    printf("ERROR: __builtin_bcdsub unexpectedly overflowed\n");
#else
    abort();
#endif

  /* Test arguments for valid/invalid */
  if (__builtin_bcdinvalid (a))
#if DEBUG
    printf("ERROR: __builtin_invalid input is unexpectedly invalid.\n");
#else
    abort();
#endif

  a[3] = 0xBB;     /* an invalid BCD digit */
  if (!__builtin_bcdinvalid (a))
#if DEBUG
    printf("ERROR: __builtin_invalid input is unexpectedly valid.\n");
#else
    abort();
#endif

  value_a = 1020304;
  a = num2bcd(value_a, 0);

  value_b = 101010;
  b = num2bcd(value_b, 0);

  /* Test equality */
  if (__builtin_bcdcmpeq (a, b))
#if DEBUG
    printf("ERROR: __builtin__bcdcmpeq result is unexpectedly 1.\n");
#else
    abort();
#endif

  if (!__builtin_bcdcmpeq (a, a))
#if DEBUG
    printf("ERROR: __builtin__bcdcmpeq result is unexpectedly 0.\n");
#else
    abort();
#endif


  /* Test a greater then b, inputs already setup this way.  */
  if (!__builtin_bcdcmpgt (a, b))
#if DEBUG
    printf("ERROR: __builtin__bcdcmpgt result is unexpectedly 0.\n");
#else
    abort();
#endif

  if (__builtin_bcdcmpgt (b, a))
#if DEBUG
    printf("ERROR: __builtin__bcdcmpgt result is unexpectedly 1.\n");
#else
    abort();
#endif

  if (__builtin_bcdcmpgt (a, a))
#if DEBUG
    printf("ERROR: __builtin__bcdcmpgt input equal, result is unexpectedly "
	   "1.\n");
#else
    abort();
#endif


  if (!__builtin_bcdcmpge (a, b))
#if DEBUG
    printf("ERROR: __builtin__bcdcmpge result is unexpectedly 0.\n");
#else
    abort();
#endif

  if (__builtin_bcdcmpge (b, a))
#if DEBUG
    printf("ERROR: __builtin__bcdcmpge result is unexpectedly 1.\n");
#else
    abort();
#endif

  if (!__builtin_bcdcmpge (b, b))
#if DEBUG
    printf("ERROR: __builtin__bcdcmpge inputs equal result is unexpectedly "
	   "0.\n");
#else
    abort();
#endif

  /* Test a less then b.  */
  value_a = 101010;
  a = num2bcd(value_a, 0);
  value_b = 1020304;
  b = num2bcd(value_b, 0);

  if (!__builtin_bcdcmplt (a, b))
#if DEBUG
    printf("ERROR: __builtin__bcdcmplt result is unexpectedly 0.\n");
#else
    abort();
#endif

  if (__builtin_bcdcmplt (b, a))
#if DEBUG
    printf("ERROR: __builtin__bcdcmplt result is unexpectedly 1.\n");
#else
    abort();
#endif

  if (__builtin_bcdcmplt (b, b))
#if DEBUG
    printf("ERROR: __builtin__bcdcmplt inputs equal result is unexpectedly "
	   "1.\n");
#else
    abort();
#endif


  if (!__builtin_bcdcmple (a, b))
#if DEBUG
    printf("ERROR: __builtin__bcdcmple result is unexpectedly 0.\n");
#else
    abort();
#endif

  if (__builtin_bcdcmple (b, a))
#if DEBUG
    printf("ERROR: __builtin__bcdcmple result is unexpectedly 1.\n");
#else
    abort();
#endif

  if (!__builtin_bcdcmple (a, a))
#if DEBUG
    printf("ERROR: __builtin__bcdcmple inputs equal result is unexpectedly "
	   "0.\n");
#else
    abort();
#endif

  /* Test multipy 10 */
  value_a = 1020304;
  a = num2bcd(value_a, 0);

  value_result = value_a * 10;
  exp_result = num2bcd(value_result, 0);
  
  result = __builtin_bcdmul10 (a);

  for (i = 0; i < 16; i++)
    if (exp_result[i] != result[i]) {
#if DEBUG
      printf("ERROR:carll __builtin_bcdmul10, pos result[%d] = %d does not "
	     "match expected_result[%d] = %d\n",
	     i, result[i], i, exp_result[i]);
#else
      abort();
#endif
    }

  /* result should be positive */
#ifdef __BIG_ENDIAN__
  if ((result[15] & 0xF) != BCD_POS0)
#else
  if ((result[0] & 0xF) != BCD_POS0)
#endif
#if DEBUG
    printf("ERROR: __builtin_bcdmul10 sign, result is %d.  Does not match "
	   "expected_result = %d\n",
	   result[0] & 0xF, BCD_POS1);
#else
    abort();
#endif

  /* Test divide 10 */
  value_a = 1020304;
  a = num2bcd(value_a, 0);

  value_result = value_a / 10;
  exp_result = num2bcd(value_result, 0);
  
  result = __builtin_bcddiv10 (a);

  for (i = 0; i < 16; i++)
    if (exp_result[i] != result[i]) {
#if DEBUG
      printf("ERROR:carll __builtin_bcddiv10, pos result[%d] = %d does not "
	     "match expected_result[%d] = %d\n",
	     i, result[i], i, exp_result[i]);
#else
      abort();
#endif
    }

  /* result should be positive */
#ifdef __BIG_ENDIAN__
  if ((result[15] & 0xF) != BCD_POS0)
#else
  if ((result[0] & 0xF) != BCD_POS0)
#endif
#if DEBUG
    printf("ERROR: __builtin_bcddiv10 sign, result is %d.  Does not match "
	   "expected_result = %d\n",
	     result[0] & 0xF, BCD_POS1);
#else
    abort();
#endif

   value_a = 1020304;
   exp_result_d128 = 1020304;
   a = num2bcd(value_a, 0);

   conv.ch = a;
   conv.d128 = __builtin_bcd2dfp (a);
   result_d128 = conv.d128;
   
   if (result_d128 != exp_result_d128)
#if DEBUG
     printf("ERROR: __builtin_bcd2dfp, result does not match expected_result."
	    "\n");
#else
     abort();
#endif
     return 0;
}

