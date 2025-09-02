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

#define TEST(T, U) unsigned T test_ ## _ ## U (U x, int i) { return x[i]; }

TEST (char, V1QI)
TEST (char, V2QI)
TEST (char, V4QI)
TEST (char, V8QI)
TEST (char, V16QI)

TEST (short, V1HI)
TEST (short, V2HI)
TEST (short, V4HI)
TEST (short, V8HI)

TEST (int, V1SI)
TEST (int, V2SI)
TEST (int, V4SI)
