/* { dg-do run } */
/* { dg-options "-O3 -ffast-math" } */

extern void abort (void);

#include "vect-fmax-fmin.x"

#include "vect-fmaxv-fminv.x"

#define DEFN_SETV(type) \
		void set_vector_##type (pR##type a, type n)   \
		{					      \
		  int i;				      \
		  for (i=0; i<16; i++)			      \
		    a[i] = n;				      \
		}

#define DEFN_CHECKV(type) \
		void check_vector_##type (pR##type a, pR##type vec) \
		{						    \
		  int i;					    \
		  for (i=0; i<16; i++)				    \
		    if (a[i] != vec[i])				    \
		      abort ();					    \
		}

#define TEST2(fname, type) \
			set_vector_##type (c##type, 0.0);              \
			fname##_##type (a##type, b##type);             \
			check_vector_##type (c##type, fname##_##type##_vector);

#define TEST3(fname, type) \
			set_vector_##type (c##type, 0.0);              \
			fname##_##type (a##type, b##type, c##type);    \
			check_vector_##type (c##type, fname##_##type##_vector);

#define TEST(fname, N) \
		TEST##N (fname, F32); \
		TEST##N (fname, F64);

typedef float F32;
typedef double F64;

DEFN_SETV (F32)
DEFN_SETV (F64)

DEFN_CHECKV (F32)
DEFN_CHECKV (F64)

int main (void)
{

  F32 aF32[16];
  F32 bF32[16];
  F32 cF32[16];

  F64 aF64[16];
  F64 bF64[16];
  F64 cF64[16];
  int i;

  /* Golden vectors.  */
  F32 max_F32_vector[] = { 15.0, 14.0, 13.0, 12.0, 11.0, 10.0, 9.0, 8.0,
			   8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0 };

  F64 max_F64_vector[] = { 15.0, 14.0, 13.0, 12.0, 11.0, 10.0, 9.0, 8.0,
			   8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0 };

  F32 min_F32_vector[] = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0,
			   7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0 };

  F64 min_F64_vector[] = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0,
			   7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0 };

  F32 minv_F32_value = 0.0f;
  F32 maxv_F32_value = 15.0f;

  F64 minv_F64_value = 0.0;
  F64 maxv_F64_value = 15.0;

  /* Setup input vectors.  */
  for (i=0; i<16; i++)
    {
      aF32[i] = (float)(15-i);
      bF32[i] = (float)i;
      aF64[i] = (double)(15-i);
      bF64[i] = (double)i;
    }

  TEST (max, 3);
  TEST (min, 3);

  /* Test across lanes ops.  */
  if (maxv_f32 (max_F32_vector) != maxv_F32_value)
    abort ();
  if (minv_f32 (min_F32_vector) != minv_F32_value)
    abort ();

  if (maxv_f64 (max_F64_vector) != maxv_F64_value)
    abort ();
  if (minv_f64 (min_F64_vector) != minv_F64_value)
    abort ();

  return 0;
}
