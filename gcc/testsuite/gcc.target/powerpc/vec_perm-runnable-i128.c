/* { dg-do run { target  int128 } } */
/* { dg-require-effective-target vmx_hw } */
/* { dg-options "-maltivec -O2 " } */

#include <altivec.h>

#define DEBUG 0

#if DEBUG
#include <stdio.h>
void print_i128 (unsigned __int128 val)
{
  printf(" 0x%016llx%016llx",
         (unsigned long long)(val >> 64),
         (unsigned long long)(val & 0xFFFFFFFFFFFFFFFF));
}
#endif

extern void abort (void);

union convert_union {
  vector signed __int128    s128;
  vector unsigned __int128  u128;
  char  val[16];
} convert;

int check_u128_result(vector unsigned __int128 vresult_u128,
		      vector unsigned __int128 expected_vresult_u128)
{
  /* Use a for loop to check each byte manually so the test case will
     run with ISA 2.06.

     Return 1 if they match, 0 otherwise.  */

  int i;

  union convert_union result;
  union convert_union expected;

  result.u128 = vresult_u128;
  expected.u128 = expected_vresult_u128;

  /* Check if each byte of the result and expected match. */
  for (i = 0; i < 16; i++)
    {
      if (result.val[i] != expected.val[i])
	return 0;
    }
  return 1;
}

int check_s128_result(vector signed __int128 vresult_s128,
		      vector signed __int128 expected_vresult_s128)
{
  /* Convert the arguments to unsigned, then check equality.  */
  union convert_union result;
  union convert_union expected;

  result.s128 = vresult_s128;
  expected.s128 = expected_vresult_s128;

  return check_u128_result (result.u128, expected.u128);
}


int
main (int argc, char *argv [])
{
  int i;
  
  vector signed __int128 src_va_s128;
  vector signed __int128 src_vb_s128;
  vector signed __int128 vresult_s128;
  vector signed __int128 expected_vresult_s128;

  vector unsigned __int128 src_va_u128;
  vector unsigned __int128 src_vb_u128;
  vector unsigned __int128 src_vc_u128;
  vector unsigned __int128 vresult_u128;
  vector unsigned __int128 expected_vresult_u128;

  src_va_s128 = (vector signed __int128) {0x123456789ABCDEF0};
  src_va_s128 = src_va_s128 << 64; 
  src_va_s128 |= (vector signed __int128) {0x22446688AACCEE00};
  src_vb_s128 = (vector signed __int128) {0xFEDCBA9876543210};
  src_vb_s128 = src_vb_s128 << 64;
  src_vb_s128 |= (vector signed __int128) {0x3333333333333333};

  src_va_u128 = (vector unsigned __int128) {0x13579ACE02468BDF};
  src_va_u128 = src_va_u128 << 64;
  src_va_u128 |= (vector unsigned __int128) {0x1133557799BBDD00};
  src_vb_u128 = (vector unsigned __int128) {0xA987654FEDCB3210};
  src_vb_u128 = src_vb_u128 << 64;
  src_vb_u128 |= (vector unsigned __int128) {0x5555555555555555};


  /* Signed 128-bit arguments.  */
  vresult_s128 = vec_xxpermdi (src_va_s128, src_vb_s128, 0x1);

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  /* BE expected results  */
  expected_vresult_s128 = (vector signed __int128) {0x123456789ABCDEF0};
  expected_vresult_s128 = expected_vresult_s128 << 64;
  expected_vresult_s128 |= (vector signed __int128) {0x3333333333333333};
#else
  /* LE expected results  */
  expected_vresult_s128 = (vector signed __int128) {0xFEDCBA9876543210};
  expected_vresult_s128 = expected_vresult_s128 << 64;
  expected_vresult_s128 |= (vector signed __int128) {0x22446688AACCEE00};
#endif

  if (!check_s128_result (vresult_s128, expected_vresult_s128))
#if DEBUG
    {
      printf ("ERROR, vec_xxpermdi (src_va_s128, src_vb_s128, 0x1) result does not match expected output.\n");
      printf ("  src_va_s128:     ");
      print_i128 ((unsigned __int128) src_va_s128);
      printf ("\n  src_vb_s128:     ");
      print_i128 ((unsigned __int128) src_vb_s128);
      printf ("\n  Result:          ");
      print_i128 ((unsigned __int128) vresult_s128);
      printf ("\n  Expected result: ");
      print_i128 ((unsigned __int128) expected_vresult_s128);
      printf ("\n");
    }
#else
    abort ();
#endif

  vresult_s128 = vec_xxpermdi (src_va_s128, src_vb_s128, 0x2);

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  /* BE expected results  */
  expected_vresult_s128 = (vector signed __int128) {0x22446688AACCEE00};
  expected_vresult_s128 = expected_vresult_s128 << 64;
  expected_vresult_s128 |= (vector signed __int128) {0xFEDCBA9876543210};
#else
  /* LE expected results  */
  expected_vresult_s128 = (vector signed __int128) {0x3333333333333333};
  expected_vresult_s128 = expected_vresult_s128 << 64;
  expected_vresult_s128 |= (vector signed __int128) {0x123456789ABCDEF0};
#endif

  if (!check_s128_result (vresult_s128, expected_vresult_s128))
#if DEBUG
    {
      printf ("ERROR, vec_xxpermdi (src_va_s128, src_vb_s128, 0x2) result does not match expected output.\n");
      printf ("  src_va_s128:     ");
      print_i128 ((unsigned __int128) src_va_s128);
      printf ("\n  src_vb_s128:     ");
      print_i128 ((unsigned __int128) src_vb_s128);
      printf ("\n  Result:          ");
      print_i128 ((unsigned __int128) vresult_s128);
      printf ("\n  Expected result: ");
      print_i128 ((unsigned __int128) expected_vresult_s128);
      printf ("\n");
    }
#else
    abort ();
#endif

  /* Unigned arguments.  */
  vresult_u128 = vec_xxpermdi (src_va_u128, src_vb_u128, 0x1);

  #if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  /* BE expected results */
  expected_vresult_u128 = (vector unsigned __int128) {0x13579ACE02468BDF};
  expected_vresult_u128 = expected_vresult_u128 << 64;
  expected_vresult_u128 |= (vector unsigned __int128) {0x5555555555555555};
#else
  /* LE expected results */
  expected_vresult_u128 = (vector unsigned __int128) {0xA987654FEDCB3210};
  expected_vresult_u128 = expected_vresult_u128 << 64;
  expected_vresult_u128 |= (vector unsigned __int128) {0x1133557799BBDD00};
#endif

  if (!check_u128_result (vresult_u128, expected_vresult_u128))
#if DEBUG
    {
      printf ("ERROR, vec_xxpermdi (src_va_u128, src_vb_u128, 0x1) result does not match expected output.\n");
      printf ("  src_va_s128:     ");
      print_i128 ((unsigned __int128) src_va_s128);
      printf ("\n  src_vb_s128:     ");
      print_i128 ((unsigned __int128) src_vb_s128);
      printf ("\n  Result:          ");
      print_i128 ((unsigned __int128) vresult_u128);
      printf ("\n  Expected result: ");
      print_i128 ((unsigned __int128) expected_vresult_u128);
      printf ("\n");
    }
#else
    abort ();
#endif

  /* Unigned arguments.  */
  vresult_u128 = vec_xxpermdi (src_va_u128, src_vb_u128, 0x2);

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  /* BE expected results */
  expected_vresult_u128 = (vector unsigned __int128) {0x1133557799BBDD00};
  expected_vresult_u128 = expected_vresult_u128 << 64;
  expected_vresult_u128 |= (vector unsigned __int128) {0xA987654FEDCB3210};
#else
  /* LE expected results */
  expected_vresult_u128 = (vector unsigned __int128) {0x5555555555555555};
  expected_vresult_u128 = expected_vresult_u128 << 64;
  expected_vresult_u128 |= (vector unsigned __int128) {0x13579ACE02468BDF};
#endif
  
  if (!check_u128_result (vresult_u128, expected_vresult_u128))
#if DEBUG
    {
      printf ("ERROR, vec_xxpermdi (src_va_u128, src_vb_u128, 0x2) result does not match expected output.\n");
      printf ("  src_va_s128:     ");
      print_i128 ((unsigned __int128) src_va_s128);
      printf ("\n  src_vb_s128:     ");
      print_i128 ((unsigned __int128) src_vb_s128);
      printf ("\n  Result:          ");
      print_i128 ((unsigned __int128) vresult_u128);
      printf ("\n  Expected result: ");
      print_i128 ((unsigned __int128) expected_vresult_u128);
      printf ("\n");
    }
#else
    abort ();
#endif

    return 0;
}
