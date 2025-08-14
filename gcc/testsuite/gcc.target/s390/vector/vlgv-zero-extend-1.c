/* { dg-do compile } */
/* { dg-require-effective-target s390_mvx } */
/* { dg-additional-options "-O2" } */
/* { dg-final { scan-assembler-not {\tllg?[fhc]r\t} } } */

typedef unsigned char __attribute__ ((vector_size (1))) V1QI;
typedef unsigned char __attribute__ ((vector_size (2))) V2QI;
typedef unsigned char __attribute__ ((vector_size (4))) V4QI;
typedef unsigned char __attribute__ ((vector_size (8))) V8QI;
typedef unsigned char __attribute__ ((vector_size (16))) V16QI;

typedef unsigned short __attribute__ ((vector_size (2))) V1HI;
typedef unsigned short __attribute__ ((vector_size (4))) V2HI;
typedef unsigned short __attribute__ ((vector_size (8))) V4HI;
typedef unsigned short __attribute__ ((vector_size (16))) V8HI;

typedef unsigned int __attribute__ ((vector_size (4))) V1SI;
typedef unsigned int __attribute__ ((vector_size (8))) V2SI;
typedef unsigned int __attribute__ ((vector_size (16))) V4SI;

unsigned short ushort;
unsigned int uint;

#define TEST(T, U, I) \
  unsigned T test_ ## I ## _ ## U (U x) { return x[I]; } \
  void       test_ ## I ## _ ## U ## _ushort (U x) { ushort = x[I]; } \
  void       test_ ## I ## _ ## U ## _uint (U x) { uint = x[I]; }

#define TEST1(T, U) \
  TEST(T, U, 0)

#define TEST2(T, U) \
  TEST1 (T, U) \
  TEST(T, U, 1)

#define TEST4(T, U) \
  TEST2 (T, U) \
  TEST(T, U, 2) \
  TEST(T, U, 3)

#define TEST8(T, U) \
  TEST4 (T, U) \
  TEST(T, U, 4) \
  TEST(T, U, 5) \
  TEST(T, U, 6) \
  TEST(T, U, 7)

#define TEST16(T, U) \
  TEST8 (T, U) \
  TEST(T, U, 9) \
  TEST(T, U, 10) \
  TEST(T, U, 11) \
  TEST(T, U, 12) \
  TEST(T, U, 13) \
  TEST(T, U, 14) \
  TEST(T, U, 15)

TEST1 (char, V1QI)
TEST2 (char, V2QI)
TEST4 (char, V4QI)
TEST8 (char, V8QI)
TEST16 (char, V16QI)

TEST1 (short, V1HI)
TEST2 (short, V2HI)
TEST4 (short, V4HI)
TEST8 (short, V8HI)

TEST1 (int, V1SI)
TEST2 (int, V2SI)
TEST4 (int, V4SI)
