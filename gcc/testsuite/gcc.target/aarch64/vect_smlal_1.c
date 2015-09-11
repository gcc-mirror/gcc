/* { dg-do run } */
/* { dg-options "-O3 -fno-inline -save-temps -fno-vect-cost-model -fno-ipa-icf" } */

typedef signed char S8_t;
typedef signed short S16_t;
typedef signed int S32_t;
typedef signed long long S64_t;
typedef signed char *__restrict__ pS8_t;
typedef signed short *__restrict__ pS16_t;
typedef signed int *__restrict__ pS32_t;
typedef signed long long *__restrict__ pS64_t;
typedef unsigned char U8_t;
typedef unsigned short U16_t;
typedef unsigned int U32_t;
typedef unsigned long long U64_t;
typedef unsigned char *__restrict__ pU8_t;
typedef unsigned short *__restrict__ pU16_t;
typedef unsigned int *__restrict__ pU32_t;
typedef unsigned long long *__restrict__ pU64_t;

extern void abort ();

void
test_addS64_tS32_t4 (pS64_t a, pS32_t b, pS32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] += (S64_t) b[i] * (S64_t) c[i];
}

/* { dg-final { scan-assembler "smlal\tv\[0-9\]+\.2d" } } */
/* { dg-final { scan-assembler "smlal2\tv\[0-9\]+\.2d" } } */

void
test_addS32_tS16_t8 (pS32_t a, pS16_t b, pS16_t c)
{
  int i;
  for (i = 0; i < 8; i++)
    a[i] += (S32_t) b[i] * (S32_t) c[i];
}

/* { dg-final { scan-assembler "smlal\tv\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "smlal2\tv\[0-9\]+\.4s" } } */

void
test_addS16_tS8_t16 (pS16_t a, pS8_t b, pS8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] += (S16_t) b[i] * (S16_t) c[i];
}

void
test_addS16_tS8_t16_neg0 (pS16_t a, pS8_t b, pS8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] += (S16_t) -b[i] * (S16_t) -c[i];
}

void
test_addS16_tS8_t16_neg1 (pS16_t a, pS8_t b, pS8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] -= (S16_t) b[i] * (S16_t) -c[i];
}

void
test_addS16_tS8_t16_neg2 (pS16_t a, pS8_t b, pS8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] -= (S16_t) -b[i] * (S16_t) c[i];
}

/* { dg-final { scan-assembler-times "smlal\tv\[0-9\]+\.8h" 4 } } */
/* { dg-final { scan-assembler-times "smlal2\tv\[0-9\]+\.8h" 4 } } */

void
test_subS64_tS32_t4 (pS64_t a, pS32_t b, pS32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] -= (S64_t) b[i] * (S64_t) c[i];
}

/* { dg-final { scan-assembler "smlsl\tv\[0-9\]+\.2d" } } */
/* { dg-final { scan-assembler "smlsl2\tv\[0-9\]+\.2d" } } */

void
test_subS32_tS16_t8 (pS32_t a, pS16_t b, pS16_t c)
{
  int i;
  for (i = 0; i < 8; i++)
    a[i] -= (S32_t) b[i] * (S32_t) c[i];
}

/* { dg-final { scan-assembler "smlsl\tv\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "smlsl2\tv\[0-9\]+\.4s" } } */

void
test_subS16_tS8_t16 (pS16_t a, pS8_t b, pS8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] -= (S16_t) b[i] * (S16_t) c[i];
}

void
test_subS16_tS8_t16_neg0 (pS16_t a, pS8_t b, pS8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] += (S16_t) -b[i] * (S16_t) c[i];
}

void
test_subS16_tS8_t16_neg1 (pS16_t a, pS8_t b, pS8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] += (S16_t) b[i] * (S16_t) -c[i];
}

void
test_subS16_tS8_t16_neg2 (pS16_t a, pS8_t b, pS8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] += -((S16_t) b[i] * (S16_t) c[i]);
}

void
test_subS16_tS8_t16_neg3 (pS16_t a, pS8_t b, pS8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] -= (S16_t) -b[i] * (S16_t) -c[i];
}

/* { dg-final { scan-assembler-times "smlsl\tv\[0-9\]+\.8h" 5 } } */
/* { dg-final { scan-assembler-times "smlsl2\tv\[0-9\]+\.8h" 5 } } */

void
test_addU64_tU32_t4 (pU64_t a, pU32_t b, pU32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] += (U64_t) b[i] * (U64_t) c[i];
}

/* { dg-final { scan-assembler "umlal\tv\[0-9\]+\.2d" } } */
/* { dg-final { scan-assembler "umlal2\tv\[0-9\]+\.2d" } } */

void
test_addU32_tU16_t8 (pU32_t a, pU16_t b, pU16_t c)
{
  int i;
  for (i = 0; i < 8; i++)
    a[i] += (U32_t) b[i] * (U32_t) c[i];
}

/* { dg-final { scan-assembler "umlal\tv\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "umlal2\tv\[0-9\]+\.4s" } } */

void
test_addU16_tU8_t16 (pU16_t a, pU8_t b, pU8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] += (U16_t) b[i] * (U16_t) c[i];
}

/* { dg-final { scan-assembler "umlal\tv\[0-9\]+\.8h" } } */
/* { dg-final { scan-assembler "umlal2\tv\[0-9\]+\.8h" } } */

void
test_subU64_tU32_t4 (pU64_t a, pU32_t b, pU32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] -= (U64_t) b[i] * (U64_t) c[i];
}

/* { dg-final { scan-assembler "umlsl\tv\[0-9\]+\.2d" } } */
/* { dg-final { scan-assembler "umlsl2\tv\[0-9\]+\.2d" } } */

void
test_subU32_tU16_t8 (pU32_t a, pU16_t b, pU16_t c)
{
  int i;
  for (i = 0; i < 8; i++)
    a[i] -= (U32_t) b[i] * (U32_t) c[i];
}

/* { dg-final { scan-assembler "umlsl\tv\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "umlsl2\tv\[0-9\]+\.4s" } } */

void
test_subU16_tU8_t16 (pU16_t a, pU8_t b, pU8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] -= (U16_t) b[i] * (U16_t) c[i];
}

/* { dg-final { scan-assembler "umlsl\tv\[0-9\]+\.8h" } } */
/* { dg-final { scan-assembler "umlsl2\tv\[0-9\]+\.8h" } } */


S64_t add_rS64[4] = { 6, 7, -4, -3 };
S32_t add_rS32[8] = { 6, 7, -4, -3, 10, 11, 0, 1 };
S16_t add_rS16[16] =
 { 6, 7, -4, -3, 10, 11, 0, 1, 14, 15, 4, 5, 18, 19, 8, 9 };

S64_t sub_rS64[4] = { 0, 1, 2, 3 };
S32_t sub_rS32[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
S16_t sub_rS16[16] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };

U64_t add_rU64[4] = { 0x6, 0x7, 0x2fffffffc, 0x2fffffffd };

U32_t add_rU32[8] =
{
  0x6, 0x7, 0x2fffc, 0x2fffd,
  0xa, 0xb, 0x30000, 0x30001
};

U16_t add_rU16[16] =
{
  0x6, 0x7, 0x2fc, 0x2fd, 0xa, 0xb, 0x300, 0x301,
  0xe, 0xf, 0x304, 0x305, 0x12, 0x13, 0x308, 0x309
};

U64_t sub_rU64[4] = { 0, 1, 2, 3 };
U32_t sub_rU32[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
U16_t sub_rU16[16] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };

S8_t neg_r[16] = { -6, -5, 8, 9, -2, -1, 12, 13, 2, 3, 16, 17, 6, 7, 20, 21 };

S64_t S64_ta[16] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
S32_t S32_tb[16] = { 2, 2, -2, -2, 2, 2, -2, -2, 2, 2, -2, -2, 2, 2, -2, -2 };
S32_t S32_tc[16] = { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 };

S32_t S32_ta[16] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
S16_t S16_tb[16] = { 2, 2, -2, -2, 2, 2, -2, -2, 2, 2, -2, -2, 2, 2, -2, -2 };
S16_t S16_tc[16] = { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 };

S16_t S16_ta[16] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
S8_t S8_tb[16] = { 2, 2, -2, -2, 2, 2, -2, -2, 2, 2, -2, -2, 2, 2, -2, -2 };
S8_t S8_tc[16] = { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3 };


#define CHECK(T,N,AS,US)                      \
do                                            \
  {                                           \
    for (i = 0; i < N; i++)                   \
      if (S##T##_ta[i] != AS##_r##US##T[i])   \
        abort ();                             \
  }                                           \
while (0)

#define SCHECK(T,N,AS) CHECK(T,N,AS,S)
#define UCHECK(T,N,AS) CHECK(T,N,AS,U)

#define NCHECK(RES)                           \
do                                            \
  {                                           \
    for (i = 0; i < 16; i++)                  \
      if (S16_ta[i] != RES[i])                \
        abort ();                             \
  }                                           \
while (0)


int
main ()
{
  int i;

  test_addS64_tS32_t4 (S64_ta, S32_tb, S32_tc);
  SCHECK (64, 4, add);
  test_addS32_tS16_t8 (S32_ta, S16_tb, S16_tc);
  SCHECK (32, 8, add);
  test_addS16_tS8_t16 (S16_ta, S8_tb, S8_tc);
  SCHECK (16, 16, add);
  test_subS64_tS32_t4 (S64_ta, S32_tb, S32_tc);
  SCHECK (64, 4, sub);
  test_subS32_tS16_t8 (S32_ta, S16_tb, S16_tc);
  SCHECK (32, 8, sub);
  test_subS16_tS8_t16 (S16_ta, S8_tb, S8_tc);
  SCHECK (16, 16, sub);

  test_addU64_tU32_t4 (S64_ta, S32_tb, S32_tc);
  UCHECK (64, 4, add);
  test_addU32_tU16_t8 (S32_ta, S16_tb, S16_tc);
  UCHECK (32, 8, add);
  test_addU16_tU8_t16 (S16_ta, S8_tb, S8_tc);
  UCHECK (16, 16, add);
  test_subU64_tU32_t4 (S64_ta, S32_tb, S32_tc);
  UCHECK (64, 4, sub);
  test_subU32_tU16_t8 (S32_ta, S16_tb, S16_tc);
  UCHECK (32, 8, sub);
  test_subU16_tU8_t16 (S16_ta, S8_tb, S8_tc);
  UCHECK (16, 16, sub);

  test_addS16_tS8_t16_neg0 (S16_ta, S8_tb, S8_tc);
  NCHECK (add_rS16);
  test_subS16_tS8_t16_neg0 (S16_ta, S8_tb, S8_tc);
  NCHECK (sub_rS16);
  test_addS16_tS8_t16_neg1 (S16_ta, S8_tb, S8_tc);
  NCHECK (add_rS16);
  test_subS16_tS8_t16_neg1 (S16_ta, S8_tb, S8_tc);
  NCHECK (sub_rS16);
  test_addS16_tS8_t16_neg2 (S16_ta, S8_tb, S8_tc);
  NCHECK (add_rS16);
  test_subS16_tS8_t16_neg2 (S16_ta, S8_tb, S8_tc);
  NCHECK (sub_rS16);
  test_subS16_tS8_t16_neg3 (S16_ta, S8_tb, S8_tc);
  NCHECK (neg_r);

  return 0;
}

