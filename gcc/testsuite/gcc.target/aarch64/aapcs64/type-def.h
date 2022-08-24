/* This header file defines some types that are used in the AAPCS64 tests.  */


/* 64-bit vector of 2 floats.  */
typedef float vf2_t __attribute__((vector_size (8)));

/* 128-bit vector of 4 floats.  */
typedef float vf4_t __attribute__((vector_size (16)));

/* 128-bit vector of 4 ints.  */
typedef int vi4_t __attribute__((vector_size (16)));

/* 128-bit vector of 1 quad precision float.  */
typedef long double vlf1_t __attribute__((vector_size (16)));

/* 64-bit vector of 2 decimal floats.  */
typedef _Decimal32 vdf2_t __attribute__((vector_size (8)));

/* 128-bit vector of 4 decimal floats.  */
typedef _Decimal32 vdf4_t __attribute__((vector_size (16)));

/* 128-bit vector of 1 quad precision decimal float.  */
typedef _Decimal128 vldf1_t __attribute__((vector_size (16)));

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

struct hfa_fx3_t
{
  float a;
  float b;
  float c;
};

struct hfa_f16x1_t
{
  __fp16 a;
};

struct hfa_f16x2_t
{
  __fp16 a;
  __fp16 b;
};

struct hfa_f16x3_t
{
  __fp16 a;
  __fp16 b;
  __fp16 c;
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

/* Same, with decimal floating-point types.  */
struct hfa_dfx1_t
{
  _Decimal32 a;
};

struct hfa_dfx2_t
{
  _Decimal32 a;
  _Decimal32 b;
};

struct hfa_dfx3_t
{
  _Decimal32 a;
  _Decimal32 b;
  _Decimal32 c;
};

struct hfa_ddx2_t
{
  _Decimal64 a;
  _Decimal64 b;
};

struct hfa_ddx4_t
{
  _Decimal64 a;
  _Decimal64 b;
  _Decimal64 c;
  _Decimal64 d;
};

struct hfa_dldx3_t
{
  _Decimal128 a;
  _Decimal128 b;
  _Decimal128 c;
};

struct hfa_dffs_t
{
  _Decimal32 a;
  _Decimal32 b;
  struct hfa_dfx2_t c;
};

union hfa_dunion_t
{
  struct
    {
      _Decimal32 a;
      _Decimal32 b;
    } s;
  _Decimal32 c;
};

struct non_hfa_dfx5_t
{
  _Decimal32 a;
  _Decimal32 b;
  _Decimal32 c;
  _Decimal32 d;
  _Decimal32 e;
};

struct non_hfa_dffs_t
{
  _Decimal32 a;
  _Decimal32 b;
  struct hfa_ddx2_t c;
};

struct non_hfa_dffs_2_t
{
  struct
    {
      int a;
      int b;
    } s;
  _Decimal32 c;
  _Decimal32 d;
};

struct hva_vdf2x1_t
{
  vdf2_t a;
};

struct hva_vdf2x2_t
{
  vdf2_t a;
  vdf2_t b;
};

struct non_hfa_dffd_t
{
  _Decimal32 a;
  _Decimal32 b;
  _Decimal64 c;
};

struct non_hfa_dffvf2_t
{
  _Decimal32 a;
  _Decimal32 b;
  vdf2_t c;
};

struct non_hfa_dfffd_t
{
  _Decimal32 a;
  _Decimal32 b;
  _Decimal32 c;
  _Decimal64 d;
};

union non_hfa_dunion_t
{
  _Decimal64 a;
  _Decimal32 b;
};
