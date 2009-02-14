/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <spu_intrinsics.h>

/* spu_sr */

vector unsigned short test_sr_1 (vector unsigned short ra, vector unsigned short count)
{
  return spu_sr (ra, count);
}

vector signed short test_sr_2 (vector signed short ra, vector unsigned short count)
{
  return spu_sr (ra, count);
}

vector unsigned int test_sr_3 (vector unsigned int ra, vector unsigned int count)
{
  return spu_sr (ra, count);
}

vector signed int test_sr_4 (vector signed int ra, vector unsigned int count)
{
  return spu_sr (ra, count);
}

vector unsigned short test_sr_5 (vector unsigned short ra)
{
  return spu_sr (ra, 11);
}

vector signed short test_sr_6 (vector signed short ra)
{
  return spu_sr (ra, 11);
}

vector unsigned short test_sr_7 (vector unsigned short ra, unsigned int count)
{
  return spu_sr (ra, count);
}

vector signed short test_sr_8 (vector signed short ra, unsigned int count)
{
  return spu_sr (ra, count);
}

vector unsigned int test_sr_9 (vector unsigned int ra)
{
  return spu_sr (ra, 11);
}

vector signed int test_sr_10 (vector signed int ra)
{
  return spu_sr (ra, 11);
}

vector unsigned int test_sr_11 (vector unsigned int ra, unsigned int count)
{
  return spu_sr (ra, count);
}

vector signed int test_sr_12 (vector signed int ra, unsigned int count)
{
  return spu_sr (ra, count);
}


/* spu_sra */

vector unsigned short test_sra_1 (vector unsigned short ra, vector unsigned short count)
{
  return spu_sra (ra, count);
}

vector signed short test_sra_2 (vector signed short ra, vector unsigned short count)
{
  return spu_sra (ra, count);
}

vector unsigned int test_sra_3 (vector unsigned int ra, vector unsigned int count)
{
  return spu_sra (ra, count);
}

vector signed int test_sra_4 (vector signed int ra, vector unsigned int count)
{
  return spu_sra (ra, count);
}

vector unsigned short test_sra_5 (vector unsigned short ra)
{
  return spu_sra (ra, 11);
}

vector signed short test_sra_6 (vector signed short ra)
{
  return spu_sra (ra, 11);
}

vector unsigned short test_sra_7 (vector unsigned short ra, unsigned int count)
{
  return spu_sra (ra, count);
}

vector signed short test_sra_8 (vector signed short ra, unsigned int count)
{
  return spu_sra (ra, count);
}

vector unsigned int test_sra_9 (vector unsigned int ra)
{
  return spu_sra (ra, 11);
}

vector signed int test_sra_10 (vector signed int ra)
{
  return spu_sra (ra, 11);
}

vector unsigned int test_sra_11 (vector unsigned int ra, unsigned int count)
{
  return spu_sra (ra, count);
}

vector signed int test_sra_12 (vector signed int ra, unsigned int count)
{
  return spu_sra (ra, count);
}

/* spu_srqw */

vector unsigned char test_srqw_1 (vector unsigned char ra)
{
  return spu_srqw (ra, 5);
}

vector signed char test_srqw_2 (vector signed char ra)
{
  return spu_srqw (ra, 5);
}

vector unsigned short test_srqw_3 (vector unsigned short ra)
{
  return spu_srqw (ra, 5);
}

vector signed short test_srqw_4 (vector signed short ra)
{
  return spu_srqw (ra, 5);
}

vector unsigned int test_srqw_5 (vector unsigned int ra)
{
  return spu_srqw (ra, 5);
}

vector signed int test_srqw_6 (vector signed int ra)
{
  return spu_srqw (ra, 5);
}

vector unsigned long test_srqw_7 (vector unsigned long ra)
{
  return spu_srqw (ra, 5);
}

vector signed long test_srqw_8 (vector signed long ra)
{
  return spu_srqw (ra, 5);
}

vector unsigned long long test_srqw_9 (vector unsigned long long ra)
{
  return spu_srqw (ra, 5);
}

vector signed long long test_srqw_10 (vector signed long long ra)
{
  return spu_srqw (ra, 5);
}

vector float test_srqw_11 (vector float ra)
{
  return spu_srqw (ra, 5);
}

vector double test_srqw_12 (vector double ra)
{
  return spu_srqw (ra, 5);
}

vector unsigned char test_srqw_13 (vector unsigned char ra, unsigned int count)
{
  return spu_srqw (ra, count);
}

vector signed char test_srqw_14 (vector signed char ra, unsigned int count)
{
  return spu_srqw (ra, count);
}

vector unsigned short test_srqw_15 (vector unsigned short ra, unsigned int count)
{
  return spu_srqw (ra, count);
}

vector signed short test_srqw_16 (vector signed short ra, unsigned int count)
{
  return spu_srqw (ra, count);
}

vector unsigned int test_srqw_17 (vector unsigned int ra, unsigned int count)
{
  return spu_srqw (ra, count);
}

vector signed int test_srqw_18 (vector signed int ra, unsigned int count)
{
  return spu_srqw (ra, count);
}

vector unsigned long test_srqw_19 (vector unsigned long ra, unsigned int count)
{
  return spu_srqw (ra, count);
}

vector signed long test_srqw_20 (vector signed long ra, unsigned int count)
{
  return spu_srqw (ra, count);
}

vector unsigned long long test_srqw_21 (vector unsigned long long ra, unsigned int count)
{
  return spu_srqw (ra, count);
}

vector signed long long test_srqw_22 (vector signed long long ra, unsigned int count)
{
  return spu_srqw (ra, count);
}

vector float test_srqw_23 (vector float ra, unsigned int count)
{
  return spu_srqw (ra, count);
}

vector double test_srqw_24 (vector double ra, unsigned int count)
{
  return spu_srqw (ra, count);
}

/* spu_srqwbyte */

vector unsigned char test_srqwbyte_1 (vector unsigned char ra)
{
  return spu_srqwbyte (ra, 5);
}

vector signed char test_srqwbyte_2 (vector signed char ra)
{
  return spu_srqwbyte (ra, 5);
}

vector unsigned short test_srqwbyte_3 (vector unsigned short ra)
{
  return spu_srqwbyte (ra, 5);
}

vector signed short test_srqwbyte_4 (vector signed short ra)
{
  return spu_srqwbyte (ra, 5);
}

vector unsigned int test_srqwbyte_5 (vector unsigned int ra)
{
  return spu_srqwbyte (ra, 5);
}

vector signed int test_srqwbyte_6 (vector signed int ra)
{
  return spu_srqwbyte (ra, 5);
}

vector unsigned long test_srqwbyte_7 (vector unsigned long ra)
{
  return spu_srqwbyte (ra, 5);
}

vector signed long test_srqwbyte_8 (vector signed long ra)
{
  return spu_srqwbyte (ra, 5);
}

vector unsigned long long test_srqwbyte_9 (vector unsigned long long ra)
{
  return spu_srqwbyte (ra, 5);
}

vector signed long long test_srqwbyte_10 (vector signed long long ra)
{
  return spu_srqwbyte (ra, 5);
}

vector float test_srqwbyte_11 (vector float ra)
{
  return spu_srqwbyte (ra, 5);
}

vector double test_srqwbyte_12 (vector double ra)
{
  return spu_srqwbyte (ra, 5);
}

vector unsigned char test_srqwbyte_13 (vector unsigned char ra, unsigned int count)
{
  return spu_srqwbyte (ra, count);
}

vector signed char test_srqwbyte_14 (vector signed char ra, unsigned int count)
{
  return spu_srqwbyte (ra, count);
}

vector unsigned short test_srqwbyte_15 (vector unsigned short ra, unsigned int count)
{
  return spu_srqwbyte (ra, count);
}

vector signed short test_srqwbyte_16 (vector signed short ra, unsigned int count)
{
  return spu_srqwbyte (ra, count);
}

vector unsigned int test_srqwbyte_17 (vector unsigned int ra, unsigned int count)
{
  return spu_srqwbyte (ra, count);
}

vector signed int test_srqwbyte_18 (vector signed int ra, unsigned int count)
{
  return spu_srqwbyte (ra, count);
}

vector unsigned long test_srqwbyte_19 (vector unsigned long ra, unsigned int count)
{
  return spu_srqwbyte (ra, count);
}

vector signed long test_srqwbyte_20 (vector signed long ra, unsigned int count)
{
  return spu_srqwbyte (ra, count);
}

vector unsigned long long test_srqwbyte_21 (vector unsigned long long ra, unsigned int count)
{
  return spu_srqwbyte (ra, count);
}

vector signed long long test_srqwbyte_22 (vector signed long long ra, unsigned int count)
{
  return spu_srqwbyte (ra, count);
}

vector float test_srqwbyte_23 (vector float ra, unsigned int count)
{
  return spu_srqwbyte (ra, count);
}

vector double test_srqwbyte_24 (vector double ra, unsigned int count)
{
  return spu_srqwbyte (ra, count);
}

/* spu_srqwbytebc */

vector unsigned char test_srqwbytebc_1 (vector unsigned char ra)
{
  return spu_srqwbytebc (ra, 40);
}

vector signed char test_srqwbytebc_2 (vector signed char ra)
{
  return spu_srqwbytebc (ra, 40);
}

vector unsigned short test_srqwbytebc_3 (vector unsigned short ra)
{
  return spu_srqwbytebc (ra, 40);
}

vector signed short test_srqwbytebc_4 (vector signed short ra)
{
  return spu_srqwbytebc (ra, 40);
}

vector unsigned int test_srqwbytebc_5 (vector unsigned int ra)
{
  return spu_srqwbytebc (ra, 40);
}

vector signed int test_srqwbytebc_6 (vector signed int ra)
{
  return spu_srqwbytebc (ra, 40);
}

vector unsigned long test_srqwbytebc_7 (vector unsigned long ra)
{
  return spu_srqwbytebc (ra, 40);
}

vector signed long test_srqwbytebc_8 (vector signed long ra)
{
  return spu_srqwbytebc (ra, 40);
}

vector unsigned long long test_srqwbytebc_9 (vector unsigned long long ra)
{
  return spu_srqwbytebc (ra, 40);
}

vector signed long long test_srqwbytebc_10 (vector signed long long ra)
{
  return spu_srqwbytebc (ra, 40);
}

vector float test_srqwbytebc_11 (vector float ra)
{
  return spu_srqwbytebc (ra, 40);
}

vector double test_srqwbytebc_12 (vector double ra)
{
  return spu_srqwbytebc (ra, 40);
}

vector unsigned char test_srqwbytebc_13 (vector unsigned char ra, unsigned int count)
{
  return spu_srqwbytebc (ra, count);
}

vector signed char test_srqwbytebc_14 (vector signed char ra, unsigned int count)
{
  return spu_srqwbytebc (ra, count);
}

vector unsigned short test_srqwbytebc_15 (vector unsigned short ra, unsigned int count)
{
  return spu_srqwbytebc (ra, count);
}

vector signed short test_srqwbytebc_16 (vector signed short ra, unsigned int count)
{
  return spu_srqwbytebc (ra, count);
}

vector unsigned int test_srqwbytebc_17 (vector unsigned int ra, unsigned int count)
{
  return spu_srqwbytebc (ra, count);
}

vector signed int test_srqwbytebc_18 (vector signed int ra, unsigned int count)
{
  return spu_srqwbytebc (ra, count);
}

vector unsigned long test_srqwbytebc_19 (vector unsigned long ra, unsigned int count)
{
  return spu_srqwbytebc (ra, count);
}

vector signed long test_srqwbytebc_20 (vector signed long ra, unsigned int count)
{
  return spu_srqwbytebc (ra, count);
}

vector unsigned long long test_srqwbytebc_21 (vector unsigned long long ra, unsigned int count)
{
  return spu_srqwbytebc (ra, count);
}

vector signed long long test_srqwbytebc_22 (vector signed long long ra, unsigned int count)
{
  return spu_srqwbytebc (ra, count);
}

vector float test_srqwbytebc_23 (vector float ra, unsigned int count)
{
  return spu_srqwbytebc (ra, count);
}

vector double test_srqwbytebc_24 (vector double ra, unsigned int count)
{
  return spu_srqwbytebc (ra, count);
}

