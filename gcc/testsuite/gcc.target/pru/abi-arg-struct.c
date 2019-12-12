/* Test call argument ABI: passing structs */

/* { dg-do run } */

#include <stdarg.h>

extern void abort (void);

struct S3 { char a[3]; };
struct S7 { char a[7]; };
struct S8 { char a[8]; };

struct S3 gs3 = {{11, 22, 33}};
struct S7 gs7 = {{1, 2, 3, 4, 5, 6, 7}};
struct S8 gs8 = {{1, 2, 3, 4, 5, 6, 7, 8}};

int test3_struct(char a0, char a1, char a2, char a3,
		 char a4, char a5, char a6, char a7,
		 char a8, char a9, char a10, char a11,
		 int ai)
{
  if (a0 != 11) return 1;
  if (a1 != 22) return 2;
  if (a2 != 33) return 3;
  if (a4 != 101) return 4;
  if (a5 != 111) return 5;
  if (a6 != 121) return 6;
  if (a8 != 55) return 8;
  if (a9 != 66) return 9;
  if (a10 != 77) return 10;

  if (ai != 55443322) return 100;

  return 0;
}

void test3(void)
{
  struct S3 s3x = { {101, 111, 121} };
  struct S3 s3y = { {55, 66, 77} };

  int (* volatile f)(struct S3, struct S3, struct S3, int) =
    (int (* volatile)(struct S3, struct S3, struct S3, int)) test3_struct;

  if (f(gs3, s3x, s3y, 55443322))
    abort();
}

int test7_struct(unsigned ai, struct S7 a0, ...)
{
  va_list ap;
  struct S7 s[3];
  int i;

  va_start (ap, a0);

  s[0] = a0;
  for (i = 1; i < 3; i++) {
      s[i] = va_arg (ap, struct S7);
  }

  va_end (ap);

  if (ai != 0xaabbccdd)
    return 1;

  if (s[0].a[0] != 1) return 1;
  if (s[0].a[1] != 2) return 1;
  if (s[0].a[2] != 3) return 1;
  if (s[0].a[3] != 4) return 1;
  if (s[0].a[4] != 5) return 1;
  if (s[0].a[5] != 6) return 1;
  if (s[0].a[6] != 7) return 1;

  if (s[1].a[0] != 11) return 1;
  if (s[1].a[1] != 12) return 1;
  if (s[1].a[2] != 13) return 1;
  if (s[1].a[3] != 14) return 1;
  if (s[1].a[4] != 15) return 1;
  if (s[1].a[5] != 16) return 1;
  if (s[1].a[6] != 17) return 1;

  if (s[2].a[0] != 22) return 1;
  if (s[2].a[1] != 23) return 1;
  if (s[2].a[2] != 24) return 1;
  if (s[2].a[3] != 25) return 1;
  if (s[2].a[4] != 26) return 1;
  if (s[2].a[5] != 27) return 1;
  if (s[2].a[6] != 28) return 1;

  return 0;
}

void test7(void)
{
  struct S7 s7x = { {11, 12, 13, 14, 15, 16, 17} };
  struct S7 s7y = { {22, 23, 24, 25, 26, 27, 28} };

  int (* volatile f)(unsigned, struct S7, struct S7, struct S7) =
    (int (* volatile)(unsigned, struct S7, struct S7, struct S7)) test7_struct;

  if (f(0xaabbccdd, gs7, s7x, s7y))
    abort();
}

int test8_struct(char a0, char a1, char a2, char a3,
		 char a4, char a5, char a6, char a7,
		 char a8, char a9, char a10, char a11,
		 char a12, char a13, char a14, char a15,
		 char a16, char a17, char a18, char a19,
		 char a20, char a21, char a22, char a23)
{
  if (a0 != 1) return 1;
  if (a1 != 2) return 1;
  if (a2 != 3) return 1;
  if (a3 != 4) return 1;
  if (a4 != 5) return 1;
  if (a5 != 6) return 1;
  if (a6 != 7) return 1;
  if (a7 != 8) return 1;

  if (a8 != 11) return 1;
  if (a9 != 12) return 1;
  if (a10 != 13) return 1;
  if (a11 != 14) return 1;
  if (a12 != 15) return 1;
  if (a13 != 16) return 1;
  if (a14 != 17) return 1;
  if (a15 != 18) return 1;

  if (a16 != 22) return 1;
  if (a17 != 23) return 1;
  if (a18 != 24) return 1;
  if (a19 != 25) return 1;
  if (a20 != 26) return 1;
  if (a21 != 27) return 1;
  if (a22 != 28) return 1;
  if (a23 != 29) return 1;

  return 0;
}

void test8(void)
{
  struct S8 s8x = { {11, 12, 13, 14, 15, 16, 17, 18} };
  struct S8 s8y = { {22, 23, 24, 25, 26, 27, 28, 29} };

  int (* volatile f)(struct S8, struct S8, struct S8) =
    (int (* volatile)(struct S8, struct S8, struct S8)) test8_struct;

  if (f(gs8, s8x, s8y))
    abort();
}

int
main (int argc, char** argv)
{
  test3();
  test7();
  test8();

  return 0;
}

