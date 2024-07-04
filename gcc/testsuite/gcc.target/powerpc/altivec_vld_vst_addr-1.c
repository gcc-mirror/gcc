/* { dg-options "-maltivec" } */
/* { dg-require-effective-target powerpc_altivec } */

/* Test vec_ld and vec_st can support both scalar and vector
   type address points, the list is:
     - address of unsigned char/short/int
     - address of signed char/short/int
     - address of float
     - address of vector unsigned char/short/int
     - address of vector signed char/short/int
     - address of vector float */
#include <altivec.h>

/* Test vec_ld can allow scalar and vector type address. */
vector unsigned char
test_vld_scalar_uc (const unsigned char *address)
{
  return __builtin_vec_ld (0, address);
}

vector unsigned short
test_vld_scalar_us (const unsigned short *address)
{
  return __builtin_vec_ld (0, address);
}

vector unsigned int
test_vld_scalar_ui (const unsigned int *address)
{
  return __builtin_vec_ld (0, address);
}

vector signed char
test_vld_scalar_sc (const signed char *address)
{
  return __builtin_vec_ld (0, address);
}

vector signed short
test_vld_scalar_ss (const signed short *address)
{
  return __builtin_vec_ld (0, address);
}

vector signed int
test_vld_scalar_si (const signed int *address)
{
  return __builtin_vec_ld (0, address);
}

vector float
test_vld_scalar_f (const float *address)
{
  return __builtin_vec_ld (0, address);
}

vector unsigned char
test_vld_vector_uc (const vector unsigned char *address)
{
  return __builtin_vec_ld (0, address);
}

vector unsigned short
test_vld_vector_us (const vector unsigned short *address)
{
  return __builtin_vec_ld (0, address);
}

vector unsigned int
test_vld_vector_ui (const vector unsigned int *address)
{
  return __builtin_vec_ld (0, address);
}

vector signed char
test_vld_vector_sc (const vector signed char *address)
{
  return __builtin_vec_ld (0, address);
}

vector signed short
test_vld_vector_ss (const vector signed short *address)
{
  return __builtin_vec_ld (0, address);
}

vector signed int
test_vld_vector_si (const vector signed int *address)
{
  return __builtin_vec_ld (0, address);
}

vector float
test_vld_vector_f (const vector float *address)
{
  return __builtin_vec_ld (0, address);
}

/* Test vec_st can allow scalar and vector type address. */

void
test_vst_scalar_uc (vector unsigned char v, unsigned char *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_scalar_us (vector unsigned short v, unsigned short *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_scalar_ui (vector unsigned int v, unsigned int *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_scalar_sc (vector signed char v, signed char *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_scalar_ss (vector signed short v, signed short *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_scalar_si (vector signed int v, signed int *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_scalar_f (vector float v, float *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_vector_uc (vector unsigned char v, vector unsigned char *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_vector_us (vector unsigned short v, vector unsigned short *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_vector_ui (vector unsigned int v, vector unsigned int *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_vector_sc (vector signed char v, vector signed char *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_vector_ss (vector signed short v, vector signed short *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_vector_si (vector signed int v, vector signed int *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_vector_f (vector float v, vector float *address)
{
  __builtin_vec_st (v, 0, address);
}

