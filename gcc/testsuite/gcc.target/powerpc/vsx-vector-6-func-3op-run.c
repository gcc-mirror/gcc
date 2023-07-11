/* { dg-do run { target lp64 } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#define DEBUG 0

/* Functional test of the one operand vector builtins.  */
#include "vsx-vector-6-func-3op.h"

/* Macros to check the results of the builtin tests.  */
#define FLOAT_CHECK(NAME)                                                 \
  f_result = vec_##NAME (f_src_a, f_src_b, f_src_c);			  \
                                                                          \
  if ((f_result[0] != f_##NAME##_expected[0])				  \
      || (f_result[1] != f_##NAME##_expected[1])			  \
      || (f_result[2] != f_##NAME##_expected[2])			  \
      || (f_result[3] != f_##NAME##_expected[3]))			  \
    {									  \
      if (DEBUG)							  \
	{								  \
	  printf("ERROR: vec_%s (float) expected value does not match\n", \
		 #NAME);						  \
	  printf("   expected[0] = %f; result[0] = %f\n",		  \
		 f_##NAME##_expected[0], f_result[0]);			  \
	  printf("   expected[1] = %f; result[1] = %f\n",		  \
		 f_##NAME##_expected[1], f_result[1]);			  \
	  printf("   expected[2] = %f; result[2] = %f\n",		  \
		 f_##NAME##_expected[2], f_result[2]);			  \
	  printf("   expected[3] = %f; result[3] = %f\n",		  \
		 f_##NAME##_expected[3], f_result[3]);			  \
	}								  \
      else								  \
	abort();							  \
    }

#define DOUBLE_CHECK(NAME)				                   \
  d_result = vec_##NAME (d_src_a, d_src_b, d_src_c);			   \
									   \
  if ((d_result[0] != d_##NAME##_expected[0])				   \
      || (d_result[1] != d_##NAME##_expected[1]))			   \
    {									   \
      if (DEBUG)							   \
	{								   \
	  printf("ERROR: vec_%s (double) expected value does not match\n", \
		 #NAME);						   \
	  printf("   expected[0] = %f; result[0] = %f\n",		   \
		 d_##NAME##_expected[0], d_result[0]);			   \
	  printf("   expected[1] = %f; result[1] = %f\n",		   \
		 d_##NAME##_expected[1], d_result[1]);			   \
	}								   \
      else								   \
	abort();							   \
    }

void __attribute__ ((noipa))
short_tests (vector short ss_src_a, vector short ss_src_b, vector int si_src_c,
	     vector unsigned short us_src_a, vector unsigned short us_src_b,
	     vector unsigned int ui_src_c, vector int si_expected,
	     vector unsigned int ui_expected)
{
  /* These tests were put into a function to ensure the compiler doesn't try to
     compute the results at compile time.  */
  vector int si_result;
  vector unsigned int ui_result;

  /* Vector multiply-sum saturated */
  ui_result = short_msums_unsigned (us_src_a, us_src_b, ui_src_c);
  if ((ui_result[0] != ui_expected[0])
      || (ui_result[1] != ui_expected[1])
      || (ui_result[2] != ui_expected[2])
      || (ui_result[3] != ui_expected[3]))
#if DEBUG
    {
      printf("ERROR: vec_msums (unsigned) expected value does not match\n");
      printf("   expected[0] = %d; result[0] = %d\n",
	     ui_expected[0], ui_result[0]);
      printf("   expected[1] = %d; result[1] = %d\n",
	     ui_expected[1], ui_result[1]);
      printf("   expected[2] = %d; result[2] = %d\n",
	     ui_expected[2], ui_result[2]);
      printf("   expected[3] = %d; result[3] = %d\n",
	     ui_expected[3], ui_result[3]);
    }
#else
  abort();
#endif
      
  si_result = short_msums_signed (ss_src_a, ss_src_b, si_src_c);
  if ((si_result[0] != si_expected[0])
      || (si_result[1] != si_expected[1])
      || (si_result[2] != si_expected[2])
      || (si_result[3] != si_expected[3]))
#if DEBUG
    {
      printf("ERROR: vec_msums (signed) expected value does not match\n");
      printf("   expected[0] = %d; result[0] = %d\n",
	     si_expected[0], si_result[0]);
      printf("   expected[1] = %d; result[1] = %d\n",
	     si_expected[1], si_result[1]);
      printf("   expected[2] = %d; result[2] = %d\n",
	     si_expected[2], si_result[2]);
      printf("   expected[3] = %d; result[3] = %d\n",
	     si_expected[3], si_result[3]);
    }
#else
  abort();
#endif
}

void __attribute__ ((noipa))
vector_sel_test (vector double d_src_a, vector double d_src_b,
		 vector unsigned long long ull_src_c,
		 vector bool long long bll_src_c ,
		 vector double d_selectb_expected,
		 vector double d_selectu_expected)
{
  vector double d_result;

  /* Vector select */
  d_result = double_sel_test (d_src_a, d_src_b, ull_src_c);

  if ((d_result[0] != d_selectu_expected[0])
      || (d_result[1] != d_selectu_expected[1]))
#if DEBUG
    {
      printf("ERROR: vec_sel (double, unsigned long long) expected value does not match\n");
      printf("   expected[0] = %f; result[0] = %f\n",
	     d_selectu_expected[0], d_result[0]);
      printf("   expected[1] = %f; result[1] = %f\n",
	     d_selectu_expected[1], d_result[1]);
    }
#else
  abort();
#endif

  d_result = bool_sel_test (d_src_a, d_src_b, bll_src_c);

  if ((d_result[0] != d_selectb_expected[0])
      || (d_result[1] != d_selectb_expected[1]))
#if DEBUG
    {
      printf("ERROR: vec_sel (double, bool long long) expected value does not match\n");
      printf("   expected[0] = %f; result[0] = %f\n",
	     d_selectb_expected[0], d_result[0]);
      printf("   expected[1] = %f; result[1] = %f\n",
	     d_selectb_expected[1], d_result[1]);
    }
#else
  abort();
#endif
}

void __attribute__ ((noipa))
vector_permute_test (vector double d_src_a, vector double d_src_b,
		     vector unsigned char uc_src_c,
		     vector double d_perm_expected)
{
  vector double d_result;

  /* Vector permute */
  d_result = double_permute_test (d_src_a, d_src_b, uc_src_c);

  if ((d_result[0] != d_perm_expected[0])
      || (d_result[1] != d_perm_expected[1]))
#if DEBUG
    {
      printf("ERROR: vec_perm (double, unsigned char) expected value does not match\n");
      printf("   expected[0] = %f; result[0] = %f\n",
	     d_perm_expected[0], d_result[0]);
      printf("   expected[1] = %f; result[1] = %f\n",
	     d_perm_expected[1], d_result[1]);
    }
#else
  abort();
#endif

}

int
main () {
  int i;

  vector unsigned char uc_src_c = {0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7,
				   0x8, 0x9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF};
  vector short ss_src_a = { 1, 2, 3, 4, 5, 6, 7, 8};
  vector short ss_src_b = { -10, 20, 30, 40, 50, 60, 70, 80};
  vector int   si_src_c = { 13, -27, 39, 48};
  vector int   si_expected = {43, 223, 649, 1178};
  vector unsigned short us_src_a = { 31, 32, 33, 34, 1, 2, 3, 4};
  vector unsigned short us_src_b = { 11, 7, 30, 90, 39, 48, 28, 64};
  vector unsigned int   ui_src_c = { 13, 17, 39, 91};

  vector unsigned int   ui_expected = {578, 4067, 174, 431};
  vector float f_src_a = { 126.0, 23.0, -338.0, 17.0};
  vector float f_src_b = { 2.0, -4.0, 1.0, 4.0};
  vector float f_src_c = { 6.0, -8.0, 7.0, 5.0};
  vector float f_madd_expected = {  258.0, -100.0, -331.0, 73.0};
  vector float f_msub_expected = { 246.0, -84.0, -345.0, 63.0};
  vector float f_result;
  
  vector unsigned long long ull_src_c = {0xFFFFFFFFFFFFFFFF,
					 0xFFFFFFFFFFFFFFFF};
  vector bool long long bll_src_c = {0, 0};
  vector double d_src_a = { 125.44, -338.56};
  vector double d_src_b = { 4.0, -2.0};
  vector double d_src_c = { 7.0, -3.0};
  vector double d_madd_expected = { 508.76, 674.12};
  vector double d_msub_expected = { 494.76, 680.12};
  vector double d_selectb_expected = { 125.44, -338.56};
  vector double d_selectu_expected = { 4.0, -2.0};
  vector double d_perm_expected = { 125.44, -338.56};
  vector double d_result;

  /* Run tests.  */
  short_tests (ss_src_a, ss_src_b, si_src_c, us_src_a, us_src_b,
	       ui_src_c, si_expected, ui_expected);

  FLOAT_CHECK (madd)
  FLOAT_CHECK (msub)

  DOUBLE_CHECK (madd)
  DOUBLE_CHECK (msub)
  
  vector_sel_test (d_src_a, d_src_b, ull_src_c, bll_src_c, d_selectb_expected,
		   d_selectu_expected);
  vector_permute_test (d_src_a, d_src_b, uc_src_c, d_perm_expected);

  return 0;
}
