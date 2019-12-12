/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mvsx" } */

/* Note that vector long long and vector double type require vsx support. */

/* Test vec_ld and vec_st can support both scalar and vector
   type address points, the list is:
     - address of unsigned long long
     - address of signed long long
     - address of double
     - address of vector unsigned long long
     - address of vector signed long long
     - address of vector double */
#include <altivec.h>

/* Test vec_ld can allow scalar and vector type address. */

vector unsigned long long
test_vld_scalar_ul (const unsigned long long *address)
{
  return __builtin_vec_ld (0, address);
}

vector signed long long
test_vld_scalar_sl (const signed long long *address)
{
  return __builtin_vec_ld (0, address);
}

vector double
test_vld_scalar_d (const double *address)
{
  return __builtin_vec_ld (0, address);
}

vector unsigned long long
test_vld_vector_ul (const vector unsigned long long *address)
{
  return __builtin_vec_ld (0, address);
}

vector signed long long
test_vld_vector_sl (const vector signed long long *address)
{
  return __builtin_vec_ld (0, address);
}

vector double
test_vld_vector_d (const vector double *address)
{
  return __builtin_vec_ld (0, address);
}

/* Test vec_st can allow scalar and vector type address. */

void
test_vst_scalar_ul (vector unsigned long long v, unsigned long long *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_scalar_sl (vector signed long long v, signed long long *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_scalar_d (vector double v, double *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_vector_ul (vector unsigned long long v,
		    vector unsigned long long *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_vector_sl (vector signed long long v, vector signed long long *address)
{
  __builtin_vec_st (v, 0, address);
}

void
test_vst_vector_d (vector double v, vector double *address)
{
  __builtin_vec_st (v, 0, address);
}

