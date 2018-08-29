/* { dg-do run } */
/* { dg-options "-O3 -fno-inline -save-temps -fno-vect-cost-model -fno-ipa-icf" } */

#pragma GCC target "+nosve"

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
test_addl_S64_S32_4 (pS64_t a, pS32_t b, pS32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] = (S64_t) b[i] + (S64_t) c[i];
}
/* "saddl\tv\[0-9\]+\.2d,\ v\[0-9\]+\.2s,\ v\[0-9\]+\.2s" */
/* "saddl2\tv\[0-9\]+\.2d,\ v\[0-9\]+\.4s,\ v\[0-9\]+\.4s" */

/* a = -b + c => a = c - b */
void
test_addl_S64_S32_4_neg0 (pS64_t a, pS32_t b, pS32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] = -(S64_t) b[i] + (S64_t) c[i];
}
/* "ssubl\tv\[0-9\]+\.2d,\ v\[0-9\]+\.2s,\ v\[0-9\]+\.2s" */
/* "ssubl2\tv\[0-9\]+\.2d,\ v\[0-9\]+\.4s,\ v\[0-9\]+\.4s" */

/* a = b + -c => a = b - c */
void
test_addl_S64_S32_4_neg1 (pS64_t a, pS32_t b, pS32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] = (S64_t) b[i] + -(S64_t) c[i];
}
/* "ssubl\tv\[0-9\]+\.2d,\ v\[0-9\]+\.2s,\ v\[0-9\]+\.2s" */
/* "ssubl2\tv\[0-9\]+\.2d,\ v\[0-9\]+\.4s,\ v\[0-9\]+\.4s" */

void
test_addl_S32_S16_8 (pS32_t a, pS16_t b, pS16_t c)
{
  int i;
  for (i = 0; i < 8; i++)
    a[i] = (S32_t) b[i] + (S32_t) c[i];
}
/* { dg-final { scan-assembler "saddl\tv\[0-9\]+\.4s,\ v\[0-9\]+\.4h,\ v\[0-9\]+\.4h" } } */
/* { dg-final { scan-assembler "saddl2\tv\[0-9\]+\.4s,\ v\[0-9\]+\.8h,\ v\[0-9\]+\.8h" } } */

void
test_addl_S16_S8_16 (pS16_t a, pS8_t b, pS8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] = (S16_t) b[i] + (S16_t) c[i];
}
/* { dg-final { scan-assembler "saddl\tv\[0-9\]+\.8h,\ v\[0-9\]+\.8b,\ v\[0-9\]+\.8b" } } */
/* { dg-final { scan-assembler "saddl2\tv\[0-9\]+\.8h,\ v\[0-9\]+\.16b,\ v\[0-9\]+\.16b" } } */

void
test_addl_U64_U32_4 (pU64_t a, pU32_t b, pU32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] = (U64_t) b[i] + (U64_t) c[i];
}
/* { dg-final { scan-assembler "uaddl\tv\[0-9\]+\.2d,\ v\[0-9\]+\.2s,\ v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "uaddl2\tv\[0-9\]+\.2d,\ v\[0-9\]+\.4s,\ v\[0-9\]+\.4s" } } */

void
test_addl_U32_U16_8 (pU32_t a, pU16_t b, pU16_t c)
{
  int i;
  for (i = 0; i < 8; i++)
    a[i] = (U32_t) b[i] + (U32_t) c[i];
}
/* { dg-final { scan-assembler "uaddl\tv\[0-9\]+\.4s,\ v\[0-9\]+\.4h,\ v\[0-9\]+\.4h" } } */
/* { dg-final { scan-assembler "uaddl2\tv\[0-9\]+\.4s,\ v\[0-9\]+\.8h,\ v\[0-9\]+\.8h" } } */

void
test_addl_U16_U8_16 (pU16_t a, pU8_t b, pU8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] = (U16_t) b[i] + (U16_t) c[i];
}
/* { dg-final { scan-assembler "uaddl\tv\[0-9\]+\.8h,\ v\[0-9\]+\.8b,\ v\[0-9\]+\.8b" } } */
/* { dg-final { scan-assembler "uaddl2\tv\[0-9\]+\.8h,\ v\[0-9\]+\.16b,\ v\[0-9\]+\.16b" } } */

void
test_subl_S64_S32_4 (pS64_t a, pS32_t b, pS32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] = (S64_t) b[i] - (S64_t) c[i];
}
/* "ssubl\tv\[0-9\]+\.2d,\ v\[0-9\]+\.2s,\ v\[0-9\]+\.2s" */
/* "ssubl2\tv\[0-9\]+\.2d,\ v\[0-9\]+\.4s,\ v\[0-9\]+\.4s" */

/* a = b - -c => a = b + c */
void
test_subl_S64_S32_4_neg0 (pS64_t a, pS32_t b, pS32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] = (S64_t) b[i] - -(S64_t) c[i];
}
/* { dg-final { scan-assembler-times "saddl\tv\[0-9\]+\.2d,\ v\[0-9\]+\.2s,\ v\[0-9\]+\.2s" 2 } } */
/* { dg-final { scan-assembler-times "saddl2\tv\[0-9\]+\.2d,\ v\[0-9\]+\.4s,\ v\[0-9\]+\.4s" 2 } } */

/* a = -b - -c => a = c - b */
void
test_subl_S64_S32_4_neg1 (pS64_t a, pS32_t b, pS32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] = -(S64_t) b[i] - -(S64_t) c[i];
}
/* "ssubl\tv\[0-9\]+\.2d,\ v\[0-9\]+\.2s,\ v\[0-9\]+\.2s" */
/* "ssubl2\tv\[0-9\]+\.2d,\ v\[0-9\]+\.4s,\ v\[0-9\]+\.4s" */

/* a = -(b - c) => a = c - b */
void
test_subl_S64_S32_4_neg2 (pS64_t a, pS32_t b, pS32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] = -((S64_t) b[i] - (S64_t) c[i]);
}
/* { dg-final { scan-assembler-times "ssubl\tv\[0-9\]+\.2d,\ v\[0-9\]+\.2s,\ v\[0-9\]+\.2s" 5 } } */
/* { dg-final { scan-assembler-times "ssubl2\tv\[0-9\]+\.2d,\ v\[0-9\]+\.4s,\ v\[0-9\]+\.4s" 5 } } */

void
test_subl_S32_S16_8 (pS32_t a, pS16_t b, pS16_t c)
{
  int i;
  for (i = 0; i < 8; i++)
    a[i] = (S32_t) b[i] - (S32_t) c[i];
}
/* { dg-final { scan-assembler "ssubl\tv\[0-9\]+\.4s,\ v\[0-9\]+\.4h,\ v\[0-9\]+\.4h" } } */
/* { dg-final { scan-assembler "ssubl2\tv\[0-9\]+\.4s,\ v\[0-9\]+\.8h,\ v\[0-9\]+\.8h" } } */

void
test_subl_S16_S8_16 (pS16_t a, pS8_t b, pS8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] = (S16_t) b[i] - (S16_t) c[i];
}
/* { dg-final { scan-assembler "ssubl\tv\[0-9\]+\.8h,\ v\[0-9\]+\.8b,\ v\[0-9\]+\.8b" } } */
/* { dg-final { scan-assembler "ssubl2\tv\[0-9\]+\.8h,\ v\[0-9\]+\.16b,\ v\[0-9\]+\.16b" } } */

void
test_subl_U64_U32_4 (pU64_t a, pU32_t b, pU32_t c)
{
  int i;
  for (i = 0; i < 4; i++)
    a[i] = (U64_t) b[i] - (U64_t) c[i];
}
/* { dg-final { scan-assembler "usubl\tv\[0-9\]+\.2d,\ v\[0-9\]+\.2s,\ v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "usubl2\tv\[0-9\]+\.2d,\ v\[0-9\]+\.4s,\ v\[0-9\]+\.4s" } } */

void
test_subl_U32_U16_8 (pU32_t a, pU16_t b, pU16_t c)
{
  int i;
  for (i = 0; i < 8; i++)
    a[i] = (U32_t) b[i] - (U32_t) c[i];
}
/* { dg-final { scan-assembler "usubl\tv\[0-9\]+\.4s,\ v\[0-9\]+\.4h,\ v\[0-9\]+\.4h" } } */
/* { dg-final { scan-assembler "usubl2\tv\[0-9\]+\.4s,\ v\[0-9\]+\.8h,\ v\[0-9\]+\.8h" } } */

void
test_subl_U16_U8_16 (pU16_t a, pU8_t b, pU8_t c)
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] = (U16_t) b[i] - (U16_t) c[i];
}
/* { dg-final { scan-assembler "usubl\tv\[0-9\]+\.8h,\ v\[0-9\]+\.8b,\ v\[0-9\]+\.8b" } } */
/* { dg-final { scan-assembler "usubl2\tv\[0-9\]+\.8h,\ v\[0-9\]+\.16b,\ v\[0-9\]+\.16b" } } */

/* input values */

S64_t S64_ta[4];
S32_t S32_tb[4] = { 0, 1, 2, 3 };
S32_t S32_tc[4] = { 2, 2, -2, -2 };

S32_t S32_ta[8];
S16_t S16_tb[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
S16_t S16_tc[8] = { 2, 2, -2, -2, 2, 2, -2, -2 };

S16_t S16_ta[16];
S8_t S8_tb[16] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
S8_t S8_tc[16] = { 2, 2, -2, -2, 2, 2, -2, -2, 2, 2, -2, -2, 2, 2, -2, -2 };

/* expected output */

S64_t addl_rS64[] = { 2, 3, 0, 1 };
S64_t neg_r[] = { 2, 1, -4, -5 };
S32_t addl_rS32[] = { 2, 3, 0, 1, 6, 7, 4, 5 };
S16_t addl_rS16[] = { 2, 3, 0, 1, 6, 7, 4, 5, 10, 11, 8, 9, 14, 15, 12, 13 };
S64_t subl_rS64[] = { -2, -1, 4, 5 };
S32_t subl_rS32[] = { -2, -1, 4, 5, 2, 3, 8, 9 };
S16_t subl_rS16[] =
  { -2, -1, 4, 5, 2, 3, 8, 9, 6, 7, 12, 13, 10, 11, 16, 17 };
U64_t addl_rU64[] = { 2, 3, 0x100000000, 0x100000001 };
U32_t addl_rU32[] = { 2, 3, 0x10000, 0x10001, 6, 7, 0x10004, 0x10005 };
U16_t addl_rU16[] =
{
  0x0002, 0x0003, 0x0100, 0x0101, 0x0006, 0x0007, 0x0104, 0x0105,
  0x000a, 0x000b, 0x0108, 0x0109, 0x000e, 0x000f, 0x010c, 0x010d
};
U64_t subl_rU64[] =
{
  0xfffffffffffffffe, 0xffffffffffffffff,
  0xffffffff00000004, 0xffffffff00000005
};
U32_t subl_rU32[] =
{
  0xfffffffe, 0xffffffff, 0xffff0004, 0xffff0005,
  0x00000002, 0x00000003, 0xffff0008, 0xffff0009
};
U16_t subl_rU16[] =
{
  0xfffe, 0xffff, 0xff04, 0xff05, 0x0002, 0x0003, 0xff08, 0xff09,
  0x0006, 0x0007, 0xff0c, 0xff0d, 0x000a, 0x000b, 0xff10, 0xff11
};

#define CHECK(T,N,AS,US)                                 \
do                                                       \
  {                                                      \
    for (i = 0; i < N; i++)                              \
      if ((US##T##_t)S##T##_ta[i] != AS##_##r##US##T[i]) \
        abort();                                         \
  }                                                      \
while (0)

#define NCHECK(RES)                           \
do                                            \
  {                                           \
    for (i = 0; i < 4; i++)                   \
      if (S64_ta[i] != RES[i])                \
        abort ();                             \
  }                                           \
while (0)

#define SCHECK(T,N,AS) CHECK(T,N,AS,S)
#define UCHECK(T,N,AS) CHECK(T,N,AS,U)

int
main ()
{
  int i;

  test_addl_S64_S32_4 (S64_ta, S32_tb, S32_tc);
  SCHECK (64, 4, addl);
  test_addl_S32_S16_8 (S32_ta, S16_tb, S16_tc);
  SCHECK (32, 8, addl);
  test_addl_S16_S8_16 (S16_ta, S8_tb, S8_tc);
  SCHECK (16, 16, addl);
  test_subl_S64_S32_4 (S64_ta, S32_tb, S32_tc);
  SCHECK (64, 4, subl);
  test_subl_S32_S16_8 (S32_ta, S16_tb, S16_tc);
  SCHECK (32, 8, subl);
  test_subl_S16_S8_16 (S16_ta, S8_tb, S8_tc);
  SCHECK (16, 16, subl);

  test_addl_U64_U32_4 (S64_ta, S32_tb, S32_tc);
  UCHECK (64, 4, addl);
  test_addl_U32_U16_8 (S32_ta, S16_tb, S16_tc);
  UCHECK (32, 8, addl);
  test_addl_U16_U8_16 (S16_ta, S8_tb, S8_tc);
  UCHECK (16, 16, addl);
  test_subl_U64_U32_4 (S64_ta, S32_tb, S32_tc);
  UCHECK (64, 4, subl);
  test_subl_U32_U16_8 (S32_ta, S16_tb, S16_tc);
  UCHECK (32, 8, subl);
  test_subl_U16_U8_16 (S16_ta, S8_tb, S8_tc);
  UCHECK (16, 16, subl);

  test_addl_S64_S32_4_neg0 (S64_ta, S32_tb, S32_tc);
  NCHECK (neg_r);
  test_addl_S64_S32_4_neg1 (S64_ta, S32_tb, S32_tc);
  NCHECK (subl_rS64);
  test_subl_S64_S32_4_neg0 (S64_ta, S32_tb, S32_tc);
  NCHECK (addl_rS64);
  test_subl_S64_S32_4_neg1 (S64_ta, S32_tb, S32_tc);
  NCHECK (neg_r);
  test_subl_S64_S32_4_neg2 (S64_ta, S32_tb, S32_tc);
  NCHECK (neg_r);

  return 0;
}


