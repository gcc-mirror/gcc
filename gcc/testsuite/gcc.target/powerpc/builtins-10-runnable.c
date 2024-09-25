/* { dg-do run { target int128 } } */
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
  vector bool __int128  b128;
  char  val[16];
} convert;

int check_u128_result(vector unsigned __int128 vresult_u128,
		      vector unsigned __int128 expected_vresult_u128)
{
  /* Use a for loop to check each byte manually so the test case will run
     with ISA 2.06.

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

int check_b128_result(vector bool __int128 vresult_b128,
		      vector bool __int128 expected_vresult_b128)
{
  /* Convert the arguments to unsigned, then check equality.  */
  union convert_union result;
  union convert_union expected;

  result.b128 = vresult_b128;
  expected.b128 = expected_vresult_b128;

  return check_u128_result (result.u128, expected.u128);
}


int
main (int argc, char *argv [])
{
  int i;
  
  vector signed __int128 src_va_s128;
  vector signed __int128 src_vb_s128;
  vector signed __int128 src_vc_s128;
  vector signed __int128 vresult_s128;
  vector signed __int128 expected_vresult_s128;

  vector unsigned __int128 src_va_u128;
  vector unsigned __int128 src_vb_u128;
  vector unsigned __int128 src_vc_u128;
  vector unsigned __int128 vresult_u128;
  vector unsigned __int128 expected_vresult_u128;

  vector bool __int128 src_va_b128;
  vector bool __int128 src_vb_b128;
  vector bool __int128 src_vc_b128;
  vector bool __int128 vresult_b128;
  vector bool __int128 expected_vresult_b128;

  src_va_s128 = (vector signed __int128) {0x123456789ABCDEF0};
  src_vb_s128 = (vector signed __int128) {0xFEDCBA9876543210};
  src_vc_b128 = (vector bool   __int128) {0x3333333333333333};
  src_vc_u128 = (vector unsigned __int128) {0xBBBBBBBBBBBBBBBB};

  /* Signed arguments.  */
  expected_vresult_s128 = (vector signed __int128) {0x32147658ba9cfed0};
  vresult_s128 = vec_sel (src_va_s128, src_vb_s128, src_vc_b128);

  if (!check_s128_result (vresult_s128, expected_vresult_s128))
#if DEBUG
    {
      printf ("ERROR, vec_sel (src_va_s128, src_vb_s128, src_vc_b128) result does not match expected output.\n");
      printf ("  Result:          ");
      print_i128 ((unsigned __int128) vresult_s128);
      printf ("\n  Expected result: ");
      print_i128 ((unsigned __int128) expected_vresult_s128);
      printf ("\n");
    }
#else
    abort ();
#endif

  expected_vresult_s128 = (vector signed __int128) {0xba9cfed832147650};
  vresult_s128 = vec_sel (src_va_s128, src_vb_s128, src_vc_u128);

  if (!check_s128_result (vresult_s128, expected_vresult_s128))
#if DEBUG
    {
      printf ("ERROR, vec_sel (src_va_s128, src_vb_s128, src_vc_u128) result does not match expected output.\n");
      printf ("  Result:          ");
      print_i128 ((unsigned __int128) vresult_s128);
      printf ("\n  Expected result: ");
      print_i128 ((unsigned __int128) expected_vresult_s128);
      printf ("\n");
    }
#else
    abort ();
#endif

  src_va_u128 = (vector unsigned __int128) {0x13579ACE02468BDF};
  src_va_b128 = (vector bool __int128) {0xFFFFFFFF00000000};
  src_vb_u128 = (vector unsigned __int128) {0xA987654FEDCB3210};
  src_vb_b128 = (vector bool __int128) {0xFFFF0000FFFF0000};
  src_vc_u128 = (vector unsigned __int128) {0x5555555555555555};

  /* Unsigned arguments.  */
  expected_vresult_u128 = (vector unsigned __int128) {0x2147a9cf2147badc};
  vresult_u128 = vec_sel (src_va_u128, src_vb_u128, src_vc_b128);

  if (!check_u128_result (vresult_u128, expected_vresult_u128))
#if DEBUG
    {
      printf ("ERROR, vec_sel (src_va_u128, src_vb_u128, src_vc_b128) result does not match expected output.\n");
      printf ("  Result:          ");
      print_i128 ((unsigned __int128) vresult_u128);
      printf ("\n  Expected result: ");
      print_i128 ((unsigned __int128) expected_vresult_u128);
      printf ("\n");
    }
#else
    abort ();
#endif

  expected_vresult_u128 = (vector unsigned __int128) {0x307cfcf47439a9a};
  vresult_u128 = vec_sel (src_va_u128, src_vb_u128, src_vc_u128);

  if (!check_u128_result (vresult_u128, expected_vresult_u128))
#if DEBUG
    {
      printf ("ERROR, vec_sel (src_va_u128, src_vb_u128, src_vc_u128) result does not match expected output.\n");
      printf ("  Result:          ");
      print_i128 ((unsigned __int128) vresult_u128);
      printf ("\n  Expected result: ");
      print_i128 ((unsigned __int128) expected_vresult_u128);
      printf ("\n");
    }
#else
    abort ();
#endif

  /* Boolean arguments.  */
  expected_vresult_b128 = (vector bool __int128) {0xffffcccc33330000};
  vresult_b128 = vec_sel (src_va_b128, src_vb_b128, src_vc_b128);

  if (!check_b128_result (vresult_b128, expected_vresult_b128))
#if DEBUG
    {
      printf ("ERROR, vec_sel (src_va_b128, src_vb_b128, src_vc_b128) result does not match expected output.\n");
      printf ("  Result:          ");
      print_i128 ((unsigned __int128) vresult_b128);
      printf ("\n  Expected result: ");
      print_i128 ((unsigned __int128) expected_vresult_b128);
      printf ("\n");
    }
#else
    abort ();
#endif

  expected_vresult_b128 = (vector bool __int128) {0xffffaaaa55550000};
  vresult_b128 = vec_sel (src_va_b128, src_vb_b128, src_vc_u128);

  if (!check_b128_result (vresult_b128, expected_vresult_b128))
#if DEBUG
    {
      printf ("ERROR, vec_sel (src_va_b128, src_vb_b128, src_vc_u128) result does not match expected output.\n");
      printf ("  Result:          ");
      print_i128 ((unsigned __int128) vresult_b128);
      printf ("\n  Expected result: ");
      print_i128 ((unsigned __int128) expected_vresult_b128);
      printf ("\n");
    }
#else
    abort ();
#endif

    return 0;
}
