/* { dg-do assemble { target { aarch64_asm_sve_ok && { ! ilp32 } } } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps" } */

typedef __INT8_TYPE__ vnx16qi __attribute__((vector_size(32)));
typedef __INT16_TYPE__ vnx8hi __attribute__((vector_size(32)));
typedef __INT32_TYPE__ vnx4si __attribute__((vector_size(32)));
typedef __INT64_TYPE__ vnx2di __attribute__((vector_size(32)));

typedef __UINT8_TYPE__ v32qu __attribute__((vector_size(32)));
typedef __UINT16_TYPE__ v16hu __attribute__((vector_size(32)));
typedef __UINT32_TYPE__ v8su __attribute__((vector_size(32)));
typedef __UINT64_TYPE__ v4du __attribute__((vector_size(32)));

#define DEF_VCOND_VAR(TYPE, COND, SUFFIX)			\
TYPE vcond_##TYPE##_##SUFFIX (TYPE x, TYPE y, TYPE a, TYPE b)	\
{								\
  TYPE r;							\
  r = a COND b ? x : y;						\
  return r;							\
}

#define DEF_VCOND_IMM(TYPE, COND, IMM, SUFFIX)			\
TYPE vcond_imm_##TYPE##_##SUFFIX (TYPE x, TYPE y, TYPE a)	\
{								\
  TYPE r;							\
  r = a COND IMM ? x : y;					\
  return r;							\
}

#define TEST_COND_VAR_SIGNED_ALL(T, COND, SUFFIX)	\
  T (vnx16qi, COND, SUFFIX)				\
  T (vnx8hi, COND, SUFFIX)				\
  T (vnx4si, COND, SUFFIX)				\
  T (vnx2di, COND, SUFFIX)

#define TEST_COND_VAR_UNSIGNED_ALL(T, COND, SUFFIX)	\
  T (v32qu, COND, SUFFIX)				\
  T (v16hu, COND, SUFFIX)				\
  T (v8su, COND, SUFFIX)				\
  T (v4du, COND, SUFFIX)

#define TEST_COND_VAR_ALL(T, COND, SUFFIX)		\
  TEST_COND_VAR_SIGNED_ALL (T, COND, SUFFIX)		\
  TEST_COND_VAR_UNSIGNED_ALL (T, COND, SUFFIX)

#define TEST_VAR_ALL(T)				\
  TEST_COND_VAR_ALL (T, >, gt)			\
  TEST_COND_VAR_ALL (T, <, lt)			\
  TEST_COND_VAR_ALL (T, >=, ge)			\
  TEST_COND_VAR_ALL (T, <=, le)			\
  TEST_COND_VAR_ALL (T, ==, eq)			\
  TEST_COND_VAR_ALL (T, !=, ne)

#define TEST_COND_IMM_SIGNED_ALL(T, COND, IMM, SUFFIX)	\
  T (vnx16qi, COND, IMM, SUFFIX)				\
  T (vnx8hi, COND, IMM, SUFFIX)				\
  T (vnx4si, COND, IMM, SUFFIX)				\
  T (vnx2di, COND, IMM, SUFFIX)

#define TEST_COND_IMM_UNSIGNED_ALL(T, COND, IMM, SUFFIX)	\
  T (v32qu, COND, IMM, SUFFIX)					\
  T (v16hu, COND, IMM, SUFFIX)					\
  T (v8su, COND, IMM, SUFFIX)					\
  T (v4du, COND, IMM, SUFFIX)

#define TEST_COND_IMM_ALL(T, COND, IMM, SUFFIX)		\
  TEST_COND_IMM_SIGNED_ALL (T, COND, IMM, SUFFIX)	\
  TEST_COND_IMM_UNSIGNED_ALL (T, COND, IMM, SUFFIX)

#define TEST_IMM_ALL(T)							\
  /* Expect immediates to make it into the encoding.  */		\
  TEST_COND_IMM_ALL (T, >, 5, gt)					\
  TEST_COND_IMM_ALL (T, <, 5, lt)					\
  TEST_COND_IMM_ALL (T, >=, 5, ge)					\
  TEST_COND_IMM_ALL (T, <=, 5, le)					\
  TEST_COND_IMM_ALL (T, ==, 5, eq)					\
  TEST_COND_IMM_ALL (T, !=, 5, ne)					\
									\
  TEST_COND_IMM_SIGNED_ALL (T, >, 15, gt2)				\
  TEST_COND_IMM_SIGNED_ALL (T, <, 15, lt2)				\
  TEST_COND_IMM_SIGNED_ALL (T, >=, 15, ge2)				\
  TEST_COND_IMM_SIGNED_ALL (T, <=, 15, le2)				\
  TEST_COND_IMM_SIGNED_ALL (T, ==, 15, eq2)				\
  TEST_COND_IMM_SIGNED_ALL (T, !=, 15, ne2)				\
									\
  TEST_COND_IMM_SIGNED_ALL (T, >, -16, gt3)				\
  TEST_COND_IMM_SIGNED_ALL (T, <, -16, lt3)				\
  TEST_COND_IMM_SIGNED_ALL (T, >=, -16, ge3)				\
  TEST_COND_IMM_SIGNED_ALL (T, <=, -16, le3)				\
  TEST_COND_IMM_SIGNED_ALL (T, ==, -16, eq3)				\
  TEST_COND_IMM_SIGNED_ALL (T, !=, -16, ne3)				\
									\
  TEST_COND_IMM_UNSIGNED_ALL (T, >, 0, gt4)				\
  /* Testing if an unsigned value >= 0 or < 0 is pointless as it will	\
     get folded away by the compiler.  */				\
  TEST_COND_IMM_UNSIGNED_ALL (T, <=, 0, le4)				\
									\
  TEST_COND_IMM_UNSIGNED_ALL (T, >, 31, gt5)				\
  TEST_COND_IMM_UNSIGNED_ALL (T, <, 31, lt5)				\
  TEST_COND_IMM_UNSIGNED_ALL (T, >=, 31, ge5)				\
  TEST_COND_IMM_UNSIGNED_ALL (T, <=, 31, le5)				\
									\
  /* Expect immediates to NOT make it into the encoding, and instead be	\
     forced into a register.  */					\
  TEST_COND_IMM_ALL (T, >, 32, gt6)					\
  TEST_COND_IMM_ALL (T, <, 32, lt6)					\
  TEST_COND_IMM_ALL (T, >=, 32, ge6)					\
  TEST_COND_IMM_ALL (T, <=, 32, le6)					\
  TEST_COND_IMM_ALL (T, ==, 32, eq6)					\
  TEST_COND_IMM_ALL (T, !=, 32, ne6)

TEST_VAR_ALL (DEF_VCOND_VAR)
TEST_IMM_ALL (DEF_VCOND_IMM)

/* { dg-final { scan-assembler {\tsel\tz[0-9]+\.b, p[0-9]+, z[0-9]+\.b, z[0-9]+\.b\n} } } */
/* { dg-final { scan-assembler {\tsel\tz[0-9]+\.h, p[0-9]+, z[0-9]+\.h, z[0-9]+\.h\n} } } */
/* { dg-final { scan-assembler {\tsel\tz[0-9]+\.s, p[0-9]+, z[0-9]+\.s, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler {\tsel\tz[0-9]+\.d, p[0-9]+, z[0-9]+\.d, z[0-9]+\.d\n} } } */

/* { dg-final { scan-assembler {\tcmpgt\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} } } */
/* { dg-final { scan-assembler {\tcmpgt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */
/* { dg-final { scan-assembler {\tcmpgt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler {\tcmpgt\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} } } */

/* { dg-final { scan-assembler {\tcmphi\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} } } */
/* { dg-final { scan-assembler {\tcmphi\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */
/* { dg-final { scan-assembler {\tcmphi\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler {\tcmphi\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} } } */

/* { dg-final { scan-assembler {\tcmphs\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} } } */
/* { dg-final { scan-assembler {\tcmphs\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */
/* { dg-final { scan-assembler {\tcmphs\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler {\tcmphs\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} } } */

/* { dg-final { scan-assembler {\tcmpge\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} } } */
/* { dg-final { scan-assembler {\tcmpge\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */
/* { dg-final { scan-assembler {\tcmpge\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler {\tcmpge\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} } } */

/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} } } */
/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */
/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} } } */

/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} } } */
/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} } } */
/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} } } */



/* { dg-final { scan-assembler {\tcmpgt\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #15\n} } } */
/* { dg-final { scan-assembler {\tcmpgt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #15\n} } } */
/* { dg-final { scan-assembler {\tcmpgt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #15\n} } } */
/* { dg-final { scan-assembler {\tcmpgt\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #15\n} } } */

/* { dg-final { scan-assembler {\tcmple\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #14\n} } } */
/* { dg-final { scan-assembler {\tcmple\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #14\n} } } */
/* { dg-final { scan-assembler {\tcmple\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #14\n} } } */
/* { dg-final { scan-assembler {\tcmple\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #14\n} } } */

/* { dg-final { scan-assembler {\tcmpgt\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #14\n} } } */
/* { dg-final { scan-assembler {\tcmpgt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #14\n} } } */
/* { dg-final { scan-assembler {\tcmpgt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #14\n} } } */
/* { dg-final { scan-assembler {\tcmpgt\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #14\n} } } */

/* { dg-final { scan-assembler {\tcmple\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #15\n} } } */
/* { dg-final { scan-assembler {\tcmple\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #15\n} } } */
/* { dg-final { scan-assembler {\tcmple\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #15\n} } } */
/* { dg-final { scan-assembler {\tcmple\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #15\n} } } */

/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #15\n} } } */
/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #15\n} } } */
/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #15\n} } } */
/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #15\n} } } */

/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #15\n} } } */
/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #15\n} } } */
/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #15\n} } } */
/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #15\n} } } */

/* { dg-final { scan-assembler {\tcmpge\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #-15\n} } } */
/* { dg-final { scan-assembler {\tcmpge\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #-15\n} } } */
/* { dg-final { scan-assembler {\tcmpge\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #-15\n} } } */
/* { dg-final { scan-assembler {\tcmpge\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #-15\n} } } */

/* { dg-final { scan-assembler {\tcmplt\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #-16\n} } } */
/* { dg-final { scan-assembler {\tcmplt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #-16\n} } } */
/* { dg-final { scan-assembler {\tcmplt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #-16\n} } } */
/* { dg-final { scan-assembler {\tcmplt\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #-16\n} } } */

/* { dg-final { scan-assembler {\tcmpge\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #-16\n} } } */
/* { dg-final { scan-assembler {\tcmpge\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #-16\n} } } */
/* { dg-final { scan-assembler {\tcmpge\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #-16\n} } } */
/* { dg-final { scan-assembler {\tcmpge\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #-16\n} } } */

/* { dg-final { scan-assembler {\tcmplt\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #-15\n} } } */
/* { dg-final { scan-assembler {\tcmplt\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #-15\n} } } */
/* { dg-final { scan-assembler {\tcmplt\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #-15\n} } } */
/* { dg-final { scan-assembler {\tcmplt\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #-15\n} } } */

/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #-16\n} } } */
/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #-16\n} } } */
/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #-16\n} } } */
/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #-16\n} } } */

/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #-16\n} } } */
/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #-16\n} } } */
/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #-16\n} } } */
/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #-16\n} } } */



/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #0\n} } } */
/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0\n} } } */
/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\n} } } */
/* { dg-final { scan-assembler {\tcmpne\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\n} } } */

/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #0\n} } } */
/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #0\n} } } */
/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #0\n} } } */
/* { dg-final { scan-assembler {\tcmpeq\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #0\n} } } */


/* { dg-final { scan-assembler {\tcmphi\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #31\n} } } */
/* { dg-final { scan-assembler {\tcmphi\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #31\n} } } */
/* { dg-final { scan-assembler {\tcmphi\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #31\n} } } */
/* { dg-final { scan-assembler {\tcmphi\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #31\n} } } */

/* { dg-final { scan-assembler {\tcmpls\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #30\n} } } */
/* { dg-final { scan-assembler {\tcmpls\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #30\n} } } */
/* { dg-final { scan-assembler {\tcmpls\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #30\n} } } */
/* { dg-final { scan-assembler {\tcmpls\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #30\n} } } */

/* { dg-final { scan-assembler {\tcmphi\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #30\n} } } */
/* { dg-final { scan-assembler {\tcmphi\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #30\n} } } */
/* { dg-final { scan-assembler {\tcmphi\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #30\n} } } */
/* { dg-final { scan-assembler {\tcmphi\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #30\n} } } */

/* { dg-final { scan-assembler {\tcmpls\tp[0-9]+\.b, p[0-7]/z, z[0-9]+\.b, #31\n} } } */
/* { dg-final { scan-assembler {\tcmpls\tp[0-9]+\.h, p[0-7]/z, z[0-9]+\.h, #31\n} } } */
/* { dg-final { scan-assembler {\tcmpls\tp[0-9]+\.s, p[0-7]/z, z[0-9]+\.s, #31\n} } } */
/* { dg-final { scan-assembler {\tcmpls\tp[0-9]+\.d, p[0-7]/z, z[0-9]+\.d, #31\n} } } */
