/* Test whether vec_perm with constant permutation mask gets optimized.  */

/* { dg-do compile { target { s390x-*-* } } } */
/* { dg-options "-march=z14 -O2" } */

typedef signed char v16qi __attribute__((vector_size(16)));
typedef unsigned char uv16qi __attribute__((vector_size(16)));
typedef char bv16qi __attribute__((vector_size(16),s390_vector_bool));

typedef signed short int v8hi __attribute__((vector_size(16)));
typedef unsigned short int uv8hi __attribute__((vector_size(16)));
typedef short int bv8hi __attribute__((vector_size(16),s390_vector_bool));

typedef signed int v4si __attribute__((vector_size(16)));
typedef unsigned int uv4si __attribute__((vector_size(16)));
typedef int bv4si __attribute__((vector_size(16),s390_vector_bool));

typedef signed long long v2di __attribute__((vector_size(16)));
typedef unsigned long long uv2di __attribute__((vector_size(16)));
typedef long long bv2di __attribute__((vector_size(16),s390_vector_bool));

typedef float v4sf __attribute__((vector_size(16)));
typedef double v2df __attribute__((vector_size(16)));

#define T(t)								\
  t									\
  mergehi_##t (t in)							\
  {									\
    return __builtin_s390_vec_perm (in, in,				\
				    (uv16qi)				\
				    {0,1,2,3,4,5,6,7,16,		\
				       17,18,19,20,21,22,23});		\
  }									\
  t									\
  mergelow_##t (t in)							\
  {									\
    return __builtin_s390_vec_perm (in, in,				\
				    (uv16qi)				\
				    {8,9,10,11,12,13,14,15,		\
				       24,25,26,27,28,29,30,31});	\
  }									\
  t									\
  pack_hi_##t (t in)							\
  {									\
    return __builtin_s390_vec_perm (in, in,				\
				    (uv16qi)				\
				    {1,3,5,7,9,11,13,15,		\
				       17,19,21,23,25,27,29,31});	\
  }									\
   t									\
   pack_si_##t (t in)							\
   {									\
     return __builtin_s390_vec_perm (in, in,				\
				     (uv16qi)				\
				     {2,3,6,7,10,11,14,15,		\
					18,19,22,23,26,27,30,31});	\
   }									\
   t									\
   pack_di_##t (t in)							\
   {									\
     return __builtin_s390_vec_perm (in, in,				\
				     (uv16qi)				\
				     {4,5,6,7,12,13,14,15,		\
					20,21,22,23,28,29,30,31});	\
   }

T(v16qi)
T(uv16qi)
T(bv16qi)
T(v8hi)
T(uv8hi)
T(bv8hi)
T(v4si)
T(uv4si)
T(bv4si)
T(v2di)
T(uv2di)
T(bv2di)
T(v4sf)
T(v2df)

/* { dg-final { scan-assembler-not {vperm} } } */
/* { dg-final { scan-assembler-times {vmrh} 14 } } */
/* { dg-final { scan-assembler-times {vmrl} 14 } } */
/* { dg-final { scan-assembler-times {vpkh} 14 } } */
/* { dg-final { scan-assembler-times {vpkf} 14 } } */
/* { dg-final { scan-assembler-times {vpkg} 14 } } */
