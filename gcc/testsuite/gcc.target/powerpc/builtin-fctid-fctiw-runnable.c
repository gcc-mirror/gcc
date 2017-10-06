/* { dg-do run { target { powerpc*-*-* && { lp64 && p8vector_hw } } } } */
/* { dg-options "-mcpu=power8" } */

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

long
test_bi_lrint_1 (float __A)
{
	return (__builtin_fctid (__A));
}
long
test_bi_lrint_2 (double __A)
{
	return (__builtin_fctid (__A));
}

int
test_bi_rint_1 (float __A)
{
	return (__builtin_fctiw (__A));
}

int
test_bi_rint_2 (double __A)
{
	return (__builtin_fctiw (__A));
}


int main( void)
{
  signed long lx, expected_l;
  double dy;

  signed int x, expected_i;
  float y;
  
  dy = 1.45;
  expected_l = 1;
  lx = __builtin_fctid (dy);

  if( lx != expected_l)
#ifdef DEBUG
    printf("ERROR: __builtin_fctid(dy= %f) = %ld, expected %ld\n",
	   dy, lx, expected_l);
#else
    abort();
#endif

  dy = 3.51;
  expected_l = 4;
  lx = __builtin_fctid (dy);
  
  if( lx != expected_l)
#ifdef DEBUG
    printf("ERROR: __builtin_fctid(dy= %f) = %ld, expected %ld\n",
	   dy, lx, expected_l);
#else
    abort();
#endif

  dy = 5.57;
  expected_i = 6;
  x = __builtin_fctiw (dy);

  if( x != expected_i)
#ifdef DEBUG
    printf("ERROR: __builtin_fctiw(dy= %f) = %d, expected %d\n",
	   dy, x, expected_i);
#else
    abort();
#endif

  y = 11.47;
  expected_i = 11;
  x = __builtin_fctiw (y);

  if( x != expected_i)
#ifdef DEBUG
    printf("ERROR: __builtin_fctiw(y = %f) = %d, expected %d\n",
	   y, x, expected_i);
#else
    abort();
#endif

  y = 17.77;
  expected_l = 18;
  lx = test_bi_lrint_1 (y);

  if( lx != expected_l)
#ifdef DEBUG
    printf("ERROR: function call test_bi_lrint_1 (y = %f) = %ld, expected %ld\n",
	   y, lx, expected_l);
#else
    abort();
#endif

  dy = 7.1;
  expected_l = 7;
  lx = test_bi_lrint_2 (dy);

  if( lx != expected_l)
#ifdef DEBUG
    printf("ERROR: function call test_bi_lrint_2 (dy = %f) = %ld, expected %ld\n",
	   dy, lx, expected_l);
#else
    abort();
#endif

  y = 0.001;
  expected_i = 0;
  x = test_bi_rint_1 (y);

  if( x != expected_i)
#ifdef DEBUG
    printf("ERROR: function call test_bi_rint_1 (y = %f) = %d, expected %d\n",
	   y, x, expected_i);
#else
    abort();
#endif
  
  dy = 0.9999;
  expected_i = 1;
  x = test_bi_rint_2 (dy);

  if( x != expected_i)
#ifdef DEBUG
    printf("ERROR: function call test_bi_rint_2 (dy = %f) = %d, expected %d\n",
	   dy, x, expected_i);
#else
    abort();
#endif
}
