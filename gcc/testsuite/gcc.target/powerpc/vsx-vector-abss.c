/* { dg-do run } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx -O2" } */


#include <altivec.h>
#include <stdlib.h>

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

static vector signed char
vabss_char (vector signed char arg)
{
  return vec_abss (arg);
}

static vector signed short
vabss_short (vector signed short arg)
{
  return vec_abss (arg);
}

static vector signed int
vabss_int (vector signed int arg)
{
  return vec_abss (arg);
}

int
main (int argc, char *argv[])
{
  int i;
  vector signed char val_char;
  vector signed char expected_char;
  vector signed char result_char;
  vector signed short val_short;
  vector signed short expected_short;
  vector signed short result_short;
  vector signed int val_int;
  vector signed int expected_int;
  vector signed int result_int;

  /* CHAR */
  val_char = (vector signed char) {-7, 6, -5, 4, -3, 1, 0, -0, 1, -2, 3, -5, 6, -7};
  expected_char = (vector signed char) {7, 6, 5, 4, 3, 1, 0, 0, 1, 2, 3, 5, 6, 7};

  result_char = vabss_char (val_char);

  for (i = 0; i< 16; i++)
    if (result_char[i] != expected_char[i])
#ifdef DEBUG
      printf("ERROR: vec_abss() result_char[%d] = %d, not expected_char[%d] = %d\n",
	      i, result_char[i], i, expected_char[i]);
#else
      abort ();
#endif

  /* SHORT */
  val_short = (vector signed short) {-0, 1, -2, 3, 4, -5, 6, -7};
  expected_short = (vector signed short) {0, 1, 2, 3, 4, 5, 6, 7};

  result_short = vabss_short (val_short);

  for (i = 0; i< 8; i++)
    if (result_short[i] != expected_short[i])
#ifdef DEBUG
      printf("ERROR: vec_abss() result_short[%d] = %d, not expected_short[%d] = %d\n",
	      i, result_short[i], i, expected_short[i]);
#else
      abort ();
#endif

  /* INT */
  val_int = (vector signed int) {-7, 6, -5, 4};
  expected_int = (vector signed int) {7, 6, 5, 4};

  result_int = vabss_int (val_int);

  for (i = 0; i< 4; i++) 
    if (result_int[i] != expected_int[i])
#ifdef DEBUG
      printf("ERROR: vec_abss() result_int[%d] = %d, not expected_int[%d] = %d\n",
	      i, result_int[i], i, expected_int[i]);
#else
      abort ();
#endif
      
  return 0;
}


