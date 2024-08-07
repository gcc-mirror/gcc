/* { dg-do run  { target power10_hw } } */
/* { dg-do compile  { target { ! power10_hw } } } */
/* { dg-require-effective-target int128 } */

/* Need -save-temps for dg-final scan-assembler-times at end of test.  */
/* { dg-options "-mdejagnu-cpu=power10 -save-temps" } */

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

#if DEBUG
#define ACTION_2ARG_UNSIGNED(NAME, TYPE_NAME)				\
  printf ("vec_%s (vector TYPE __int128, vector TYPE __int128) \n", #NAME); \
  printf(" src_va_s128[0] =      ");					\
  print_i128 ((unsigned __int128) src_va_##TYPE_NAME[0]);		\
  printf("\n");							\
  printf(" src_vb_uc =            0x");				\
  for (i = 0; i < 16; i++) 						\
    printf("%02x",  src_vb_uc[i]);					\
  printf("\n");							\
  printf(" vresult[0] =          ");					\
  print_i128 ((unsigned __int128) vresult[0]);				\
  printf("\n");							\
  printf(" expected_vresult[0] = ");					\
  print_i128 ((unsigned __int128) expected_vresult[0]);		\
  printf("\n");

#define ACTION_2ARG_SIGNED(NAME, TYPE_NAME)				\
  printf ("vec_%s (vector TYPE __int128, vector TYPE __int128) \n", #NAME); \
  printf(" src_va_s128[0] =      ");					\
  print_i128 ((unsigned __int128) src_va_##TYPE_NAME[0]);		\
  printf("\n");							\
  printf(" src_vb_sc =            0x");				\
  for (i = 0; i < 16; i++) 						\
    printf("%02x",  src_vb_sc[i]);					\
  printf("\n");							\
  printf(" vresult[0] =          ");					\
  print_i128 ((unsigned __int128) vresult[0]);				\
  printf("\n");							\
  printf(" expected_vresult[0] = ");					\
  print_i128 ((unsigned __int128) expected_vresult[0]);		\
  printf("\n");

#define ACTION_3ARG(NAME, TYPE_NAME, CONST)				\
  printf ("vec_%s (vector TYPE __int128, vector TYPE __int128, %s) \n",	\
    #NAME, #CONST);							\
  printf(" src_va_s128[0] =      ");					\
  print_i128 ((unsigned __int128) src_va_##TYPE_NAME[0]);		\
  printf("\n");							\
  printf(" src_vb_s128[0] =      ");					\
  print_i128 ((unsigned __int128) src_vb_##TYPE_NAME[0]);		\
  printf("\n");							\
  printf(" vresult[0] =          ");					\
  print_i128 ((unsigned __int128) vresult[0]);				\
  printf("\n");							\
  printf(" expected_vresult[0] = ");					\
  print_i128 ((unsigned __int128) expected_vresult[0]);		\
  printf("\n");

#else
#define ACTION_2ARG_UNSIGNED(NAME, TYPE_NAME)	\
  abort();

#define ACTION_2ARG_SIGNED(NAME, TYPE_NAME)	\
  abort();

#define ACTION_2ARG(NAME, TYPE_NAME)		\
  abort();

#define ACTION_3ARG(NAME, TYPE_NAME, CONST)	\
  abort();
#endif

/* Second argument of the builtin is vector unsigned char.  */
#define TEST_2ARG_UNSIGNED(NAME, TYPE, TYPE_NAME, EXP_RESULT_HI, EXP_RESULT_LO) \
  {									\
    vector TYPE __int128 vresult;					\
    vector TYPE __int128 expected_vresult;				\
    int i;								\
    									\
    expected_vresult = (vector TYPE __int128) { EXP_RESULT_HI };	\
    expected_vresult = (expected_vresult << 64) |			\
      (vector TYPE __int128) { EXP_RESULT_LO };			\
    vresult = vec_##NAME (src_va_##TYPE_NAME, src_vb_uc);		\
									\
    if (!vec_all_eq (vresult,  expected_vresult)) {			\
      ACTION_2ARG_UNSIGNED(NAME, TYPE_NAME)				\
    }									\
  }      

/* Second argument of the builtin is vector signed char.  */
#define TEST_2ARG_SIGNED(NAME, TYPE, TYPE_NAME, EXP_RESULT_HI, EXP_RESULT_LO) \
  {									\
    vector TYPE __int128 vresult;					\
    vector TYPE __int128 expected_vresult;				\
    int i;								\
    									\
    expected_vresult = (vector TYPE __int128) { EXP_RESULT_HI };	\
    expected_vresult = (expected_vresult << 64) |			\
      (vector TYPE __int128) { EXP_RESULT_LO };			\
    vresult = vec_##NAME (src_va_##TYPE_NAME, src_vb_sc);		\
									\
    if (!vec_all_eq (vresult,  expected_vresult)) {			\
      ACTION_2ARG_SIGNED(NAME, TYPE_NAME)				\
    }									\
  }      

#define TEST_3ARG(NAME, TYPE, TYPE_NAME, CONST, EXP_RESULT_HI, EXP_RESULT_LO) \
  {									\
    vector TYPE __int128 vresult;					\
    vector TYPE __int128 expected_vresult;				\
    									\
    expected_vresult = (vector TYPE __int128) { EXP_RESULT_HI };	\
    expected_vresult = (expected_vresult << 64) |			\
      (vector TYPE __int128) { EXP_RESULT_LO };			\
    vresult = vec_##NAME (src_va_##TYPE_NAME, src_vb_##TYPE_NAME, CONST);	\
									\
    if (!vec_all_eq (vresult,  expected_vresult)) {			\
      ACTION_3ARG(NAME, TYPE_NAME, CONST)				\
    }									\
  }      

int
main (int argc, char *argv [])
{
  vector signed __int128 vresult_s128;
  vector signed __int128 expected_vresult_s128;
  vector signed __int128 src_va_s128;
  vector signed __int128 src_vb_s128;
  vector unsigned __int128 vresult_u128;
  vector unsigned __int128 expected_vresult_u128;
  vector unsigned __int128 src_va_u128;
  vector unsigned __int128 src_vb_u128;
  vector signed char src_vb_sc;
  vector unsigned char src_vb_uc;

  /* 128-bit vector shift right tests, vec_srdb. */
  src_va_s128 = (vector signed __int128) {0x12345678};
  src_vb_s128 = (vector signed __int128) {0xFEDCBA90};
  TEST_3ARG(srdb, signed, s128, 4, 0x8000000000000000, 0xFEDCBA9)
  
  src_va_u128 = (vector unsigned __int128) { 0xFEDCBA98 };
  src_vb_u128 = (vector unsigned __int128) { 0x76543210};
  TEST_3ARG(srdb, unsigned, u128, 4, 0x8000000000000000, 0x07654321)

  /* 128-bit vector shift left tests, vec_sldb. */
  src_va_s128 = (vector signed __int128) {0x123456789ABCDEF0};
  src_va_s128 = (src_va_s128 << 64)
    | (vector signed __int128) {0x123456789ABCDEF0};
  src_vb_s128 = (vector signed __int128) {0xFEDCBA9876543210};
  src_vb_s128 = (src_vb_s128 << 64)
    | (vector signed __int128) {0xFEDCBA9876543210};
  TEST_3ARG(sldb, signed, s128, 4, 0x23456789ABCDEF01, 0x23456789ABCDEF0F)

  src_va_u128 = (vector unsigned __int128) {0xFEDCBA9876543210};
  src_va_u128 = src_va_u128 << 64
    | (vector unsigned __int128) {0xFEDCBA9876543210};
  src_vb_u128 = (vector unsigned __int128) {0x123456789ABCDEF0};
  src_vb_u128 = src_vb_u128 << 64
    | (vector unsigned __int128) {0x123456789ABCDEF0};
  TEST_3ARG(sldb, unsigned, u128, 4, 0xEDCBA9876543210F, 0xEDCBA98765432101)

  /* Shift left by octect tests, vec_sld.  Shift is by immediate value
     times 8. */
  src_va_s128 = (vector signed __int128) {0x123456789ABCDEF0};
  src_va_s128 = (src_va_s128 << 64)
    | (vector signed __int128) {0x123456789ABCDEF0};
  src_vb_s128 = (vector signed __int128) {0xFEDCBA9876543210};
  src_vb_s128 = (src_vb_s128 << 64)
    | (vector signed __int128) {0xFEDCBA9876543210};
  TEST_3ARG(sld, signed, s128, 4, 0x9abcdef012345678, 0x9abcdef0fedcba98)

  src_va_u128 = (vector unsigned __int128) {0xFEDCBA9876543210};
  src_va_u128 = src_va_u128 << 64
    | (vector unsigned __int128) {0xFEDCBA9876543210};
  src_vb_u128 = (vector unsigned __int128) {0x123456789ABCDEF0};
  src_vb_u128 = src_vb_u128 << 64
    | (vector unsigned __int128) {0x123456789ABCDEF0};
  TEST_3ARG(sld, unsigned, u128, 4, 0x76543210fedcba98, 0x7654321012345678)

  /* Vector left shift bytes within the vector, vec_sll. */
  src_va_s128 = (vector signed __int128) {0x123456789ABCDEF0};
  src_va_s128 = (src_va_s128 << 64)
    | (vector signed __int128) {0x123456789ABCDEF0};
  src_vb_uc = (vector unsigned char) {0x01, 0x01, 0x01, 0x01,
				      0x01, 0x01, 0x01, 0x01,
				      0x01, 0x01, 0x01, 0x01,
				      0x01, 0x01, 0x01, 0x01};
  TEST_2ARG_UNSIGNED(sll, signed, s128, 0x2468acf13579bde0,
		     0x2468acf13579bde0)

  src_va_u128 = (vector unsigned __int128) {0x123456789ABCDEF0};
  src_va_u128 = src_va_u128 << 64
    | (vector unsigned __int128) {0x123456789ABCDEF0};
  src_vb_uc = (vector unsigned char) {0x02, 0x02, 0x02, 0x02,
				      0x02, 0x02, 0x02, 0x02,
				      0x02, 0x02, 0x02, 0x02,
				      0x02, 0x02, 0x02, 0x02};
  TEST_2ARG_UNSIGNED(sll, unsigned, u128, 0x48d159e26af37bc0,
		     0x48d159e26af37bc0)

  /* Vector right shift bytes within the vector, vec_srl. */
  src_va_s128 = (vector signed __int128) {0x123456789ABCDEF0};
  src_va_s128 = (src_va_s128 << 64)
    | (vector signed __int128) {0x123456789ABCDEF0};
  src_vb_uc = (vector unsigned char) {0x01, 0x01, 0x01, 0x01,
				      0x01, 0x01, 0x01, 0x01,
				      0x01, 0x01, 0x01, 0x01,
				      0x01, 0x01, 0x01, 0x01};
  TEST_2ARG_UNSIGNED(srl, signed, s128, 0x091a2b3c4d5e6f78,
		     0x091a2b3c4d5e6f78)

  src_va_u128 = (vector unsigned __int128) {0x123456789ABCDEF0};
  src_va_u128 = src_va_u128 << 64
    | (vector unsigned __int128) {0x123456789ABCDEF0};
  src_vb_uc = (vector unsigned char) {0x02, 0x02, 0x02, 0x02,
				      0x02, 0x02, 0x02, 0x02,
				      0x02, 0x02, 0x02, 0x02,
				      0x02, 0x02, 0x02, 0x02};
  TEST_2ARG_UNSIGNED(srl, unsigned, u128, 0x48d159e26af37bc,
		     0x48d159e26af37bc)

  /* Shift left by octect tests, vec_slo.  Shift is by immediate value
     bytes.  Shift amount in bits 121:124.  */
  src_va_s128 = (vector signed __int128) {0x123456789ABCDEF0};
  src_va_s128 = (src_va_s128 << 64)
    | (vector signed __int128) {0x123456789ABCDEF0};
  /* Note vb_sc is Endian specific.  */
  /* The left shift amount is 1 byte, i.e. 1 * 8 bits.  */
#if __LITTLE_ENDIAN__
  src_vb_sc = (vector signed char) {0x1 << 3, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0};
#else
  src_vb_sc = (vector signed char) {0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x1 << 3};
#endif

  TEST_2ARG_SIGNED(slo, signed, s128, 0x3456789ABCDEF012,
		   0x3456789ABCDEF000)
  src_va_s128 = (vector signed __int128) {0x123456789ABCDEF0};
  src_va_s128 = (src_va_s128 << 64)
    | (vector signed __int128) {0x123456789ABCDEF0};

  /* Note vb_sc is Endian specific.  */
  /* The left shift amount is 2 bytes, i.e. 2 * 8 bits.  */
#if __LITTLE_ENDIAN__
  src_vb_uc = (vector unsigned char) {0x2 << 3, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0};
#else
  src_vb_uc = (vector unsigned char) {0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x2 << 3};
#endif
  TEST_2ARG_UNSIGNED(slo, signed, s128, 0x56789ABCDEF01234,
		     0x56789ABCDEF00000)

  src_va_u128 = (vector unsigned __int128) {0xFEDCBA9876543210};
  src_va_u128 = src_va_u128 << 64
    | (vector unsigned __int128) {0xFEDCBA9876543210};
  /* Note vb_sc is Endian specific.  */
  /* The left shift amount is 3 bytes, i.e. 3 * 8 bits.  */
#if __LITTLE_ENDIAN__
  src_vb_sc = (vector signed char) {0x03<<3, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x00, 0x00, 0x00, 0x0};
#else
  src_vb_sc = (vector signed char) {0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x00, 0x00, 0x00, 0x03<<3};
#endif
  TEST_2ARG_SIGNED(slo, unsigned, u128, 0x9876543210FEDCBA,
  		     0x9876543210000000)

  src_va_u128 = (vector unsigned __int128) {0xFEDCBA9876543210};
  src_va_u128 = src_va_u128 << 64
    | (vector unsigned __int128) {0xFEDCBA9876543210};
  /* Note vb_sc is Endian specific.  */
  /* The left shift amount is 4 bytes, i.e. 4 * 8 bits.  */
#if __LITTLE_ENDIAN__
  src_vb_uc = (vector unsigned char) {0x04<<3, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0};
#else
  src_vb_uc = (vector unsigned char) {0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x00, 0x00, 0x00, 0x04<<3};
#endif
  TEST_2ARG_UNSIGNED(slo, unsigned, u128, 0x76543210FEDCBA98,
  		     0x7654321000000000)

  /* Shift right by octect tests, vec_sro.  Shift is by immediate value
     times 8.  Shift amount in bits 121:124.  */
  src_va_s128 = (vector signed __int128) {0x123456789ABCDEF0};
  src_va_s128 = (src_va_s128 << 64)
    | (vector signed __int128) {0x123456789ABCDEF0};
  /* Note vb_sc is Endian specific.  */
  /* The left shift amount is 1 byte, i.e. 1 * 8 bits.  */
#if __LITTLE_ENDIAN__
  src_vb_sc = (vector signed char) {0x1 << 3, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0};
#else
  src_vb_sc = (vector signed char) {0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x1 << 3};
#endif
  TEST_2ARG_SIGNED(sro, signed, s128, 0x00123456789ABCDE, 0xF0123456789ABCDE)

  src_va_s128 = (vector signed __int128) {0x123456789ABCDEF0};
  src_va_s128 = (src_va_s128 << 64)
    | (vector signed __int128) {0x123456789ABCDEF0};
  /* Note vb_sc is Endian specific.  */
  /* The left shift amount is 1 byte, i.e. 1 * 8 bits.  */
#if __LITTLE_ENDIAN__
  src_vb_uc = (vector unsigned char) {0x2 << 3, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0};
#else
  src_vb_uc = (vector unsigned char) {0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x2 << 3};
#endif
  TEST_2ARG_UNSIGNED(sro, signed, s128, 0x0000123456789ABC,
		     0xDEF0123456789ABC)

  src_va_u128 = (vector unsigned __int128) {0xFEDCBA9876543210};
  src_va_u128 = src_va_u128 << 64
    | (vector unsigned __int128) {0xFEDCBA9876543210};
  /* Note vb_sc is Endian specific.  */
  /* The left shift amount is 4 bytes, i.e. 4 * 8 bits.  */
#if __LITTLE_ENDIAN__
  src_vb_sc = (vector signed char) {0x03<<3, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x00, 0x00, 0x00, 0x0};
#else
  src_vb_sc = (vector signed char) {0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x0, 0x0, 0x0, 0x0,
				    0x00, 0x00, 0x00, 0x03<<3};
#endif
  TEST_2ARG_SIGNED(sro, unsigned, u128, 0x000000FEDCBA9876,
		   0x543210FEDCBA9876)

  src_va_u128 = (vector unsigned __int128) {0xFEDCBA9876543210};
  src_va_u128 = src_va_u128 << 64
    | (vector unsigned __int128) {0xFEDCBA9876543210};
  /* Note vb_sc is Endian specific.  */
  /* The left shift amount is 4 bytes, i.e. 4 * 8 bits.  */
#if __LITTLE_ENDIAN__
  src_vb_uc = (vector unsigned char) {0x04<<3, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x00, 0x0};
#else
  src_vb_uc = (vector unsigned char) {0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x0, 0x0, 0x0, 0x0,
				      0x00, 0x0, 0x0, 0x04<<3};
#endif
  TEST_2ARG_UNSIGNED(sro, unsigned, u128, 0x00000000FEDCBA98,
  		     0x76543210FEDCBA98)

  /* 128-bit vector shift left tests, vec_sldw. */
  src_va_s128 = (vector signed __int128) {0x123456789ABCDEF0};
  src_va_s128 = (src_va_s128 << 64)
    | (vector signed __int128) {0x123456789ABCDEF0};
  src_vb_s128 = (vector signed __int128) {0xFEDCBA9876543210};
  src_vb_s128 = (src_vb_s128 << 64)
    | (vector signed __int128) {0xFEDCBA9876543210};
  TEST_3ARG(sldw, signed, s128, 1, 0x9ABCDEF012345678, 0x9ABCDEF0FEDCBA98)

  src_va_u128 = (vector unsigned __int128) {0x123456789ABCDEF0};
  src_va_u128 = (src_va_u128 << 64)
    | (vector unsigned __int128) {0x123456789ABCDEF0};
  src_vb_u128 = (vector unsigned __int128) {0xFEDCBA9876543210};
  src_vb_u128 = (src_vb_u128 << 64)
    | (vector unsigned __int128) {0xFEDCBA9876543210};
  TEST_3ARG(sldw, unsigned, u128, 2, 0x123456789ABCDEF0, 0xFEDCBA9876543210)


  return 0;
}

/* { dg-final { scan-assembler-times {\mvsrdbi\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvsldbi\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvsl\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvsr\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvslo\M} 4 } } */
/* { dg-final { scan-assembler-times {\mvsro\M} 4 } } */
