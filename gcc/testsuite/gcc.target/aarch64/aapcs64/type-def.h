/* This header file defines some types that are used in the AAPCS64 tests.  */


/* 64-bit vector of 2 floats.  */
typedef float vf2_t __attribute__((vector_size (8)));

/* 128-bit vector of 4 floats.  */
typedef float vf4_t __attribute__((vector_size (16)));

/* 128-bit vector of 4 ints.  */
typedef int vi4_t __attribute__((vector_size (16)));

/* signed quad-word (in an union for the convenience of initialization).  */
union int128_t
{
  __int128 i;
  struct
    {
      signed long long l64;
      signed long long h64;
    };
};

/* Homogeneous floating-point composite types.  */

struct hfa_fx1_t
{
  float a;
};

struct hfa_fx2_t
{
  float a;
  float b;
};

struct hfa_dx2_t
{
  double a;
  double b;
};

struct hfa_dx4_t
{
  double a;
  double b;
  double c;
  double d;
};

struct hfa_ldx3_t
{
  long double a;
  long double b;
  long double c;
};

struct hfa_ffs_t
{
  float a;
  float b;
  struct hfa_fx2_t c;
};

union hfa_union_t
{
  struct
    {
      float a;
      float b;
    } s;
  float c;
};

/* Non homogeneous floating-point-composite types.  */

struct non_hfa_fx5_t
{
  float a;
  float b;
  float c;
  float d;
  float e;
};

struct non_hfa_ffs_t
{
  float a;
  float b;
  struct hfa_dx2_t c;
};

struct non_hfa_ffs_2_t
{
  struct
    {
      int a;
      int b;
    } s;
  float c;
  float d;
};

struct hva_vf2x1_t
{
  vf2_t a;
};

struct hva_vf2x2_t
{
  vf2_t a;
  vf2_t b;
};

struct hva_vi4x1_t
{
  vi4_t a;
};

struct non_hfa_ffd_t
{
  float a;
  float b;
  double c;
};

struct non_hfa_ii_t
{
  int a;
  int b;
};

struct non_hfa_c_t
{
  char a;
};

struct non_hfa_ffvf2_t
{
  float a;
  float b;
  vf2_t c;
};

struct non_hfa_fffd_t
{
  float a;
  float b;
  float c;
  double d;
};

union non_hfa_union_t
{
  double a;
  float b;
};
