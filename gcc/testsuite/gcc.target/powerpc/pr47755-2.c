/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O3 -mcpu=power7" } */

/* PR 47755: Make sure compiler generates correct code for various
   V2DI constants.  */

#ifdef DEBUG
#include <stdio.h>

static int num_errors;
#define FAIL_LL(A, B) \
  (num_errors++, printf ("Fail (%i, %i)\n", (int)(A), (int)(B)))
#define FAIL_I(A, B, C, D) \
  (num_errors++, \
  printf ("Fail (%i, %i, %i, %i)\n", (int)(A), (int)(B), (int)(C), (int)(D)))

#else
extern void abort (void) __attribute__((__noreturn__));
#define FAIL_LL(A, B) abort ()
#define FAIL_I(A, B, C, D) abort ()
#endif

static test_ll (vector long long, long long, long long) __attribute__((__noinline__));

static
test_ll (vector long long v, long long a, long long b)
{
  union {
    vector long long v;
    long long ll[2];
  } u;

  u.v = v;
  if (u.ll[0] != a && u.ll[1] != b)
    FAIL_LL (a, b);
}

#define TEST_LL(A,B) test_ll ((vector long long){ (A), (B) }, (A), (B))

static test_i (vector int, int, int, int, int) __attribute__((__noinline__));

static
test_i (vector int v, int a, int b, int c, int d)
{
  union {
    vector int v;
    int i[4];
  } u;

  u.v = v;
  if (u.i[0] != a && u.i[1] != b && u.i[2] != c && u.i[3] != d)
    FAIL_I (a, b, c, d);
}

#define TEST_I(A,B,C,D) \
  test_i ((vector int){ (A), (B), (C), (D) }, (A), (B), (C), (D))

int
main (void)
{
  TEST_LL (-2LL, -2LL);
  TEST_LL (-2LL, -1LL);
  TEST_LL (-2LL,  0LL);
  TEST_LL (-2LL,  1LL);
  TEST_LL (-2LL,  2LL);

  TEST_LL (-1LL, -2LL);
  TEST_LL (-1LL, -1LL);
  TEST_LL (-1LL,  0LL);
  TEST_LL (-1LL,  1LL);
  TEST_LL (-1LL,  2LL);

  TEST_LL (0LL, -2LL);
  TEST_LL (0LL, -1LL);
  TEST_LL (0LL,  0LL);
  TEST_LL (0LL,  1LL);
  TEST_LL (0LL,  2LL);

  TEST_LL (1LL, -2LL);
  TEST_LL (1LL, -1LL);
  TEST_LL (1LL,  0LL);
  TEST_LL (1LL,  1LL);
  TEST_LL (1LL,  2LL);

  TEST_LL (2LL, -2LL);
  TEST_LL (2LL, -1LL);
  TEST_LL (2LL,  0LL);
  TEST_LL (2LL,  1LL);
  TEST_LL (2LL,  2LL);

  /* We could use VSPLTI instructions for these tests.  */
  TEST_LL (0x0101010101010101LL, 0x0101010101010101LL);
  TEST_LL (0x0001000100010001LL, 0x0001000100010001LL);
  TEST_LL (0x0000000100000001LL, 0x0000000100000001LL);

  TEST_LL (0x0404040404040404LL, 0x0404040404040404LL);
  TEST_LL (0x0004000400040004LL, 0x0004000400040004LL);
  TEST_LL (0x0000000400000004LL, 0x0000000400000004LL);

  TEST_LL (0xf8f8f8f8f8f8f8f8LL, 0xf8f8f8f8f8f8f8f8LL);
  TEST_LL (0xfff8fff8fff8fff8LL, 0xfff8fff8fff8fff8LL);
  TEST_LL (0xfffffff8fffffff8LL, 0xfffffff8fffffff8LL);

  /* We could use VSPLTI instructions for these tests.  */
  TEST_I (-2, -2, -2, -2);
  TEST_I (-1, -1, -1, -1);
  TEST_I ( 0,  0,  0,  0);
  TEST_I ( 1,  1,  1,  1);
  TEST_I ( 2,  2,  2,  2);

  TEST_I (0x01010101, 0x01010101, 0x01010101, 0x01010101);
  TEST_I (0x00010001, 0x00010001, 0x00010001, 0x00010001);

  TEST_I (0x02020202, 0x02020202, 0x02020202, 0x02020202);
  TEST_I (0x00020002, 0x00020002, 0x00020002, 0x00020002);

  TEST_I (0xf8f8f8f8, 0xf8f8f8f8, 0xf8f8f8f8, 0xf8f8f8f8);
  TEST_I (0xfff8fff8, 0xfff8fff8, 0xfff8fff8, 0xfff8fff8);

  /* non-easy constants.  */
  TEST_I (-2, -1,  0,  1);
  TEST_I ( 1,  0, -1, -2);

  TEST_I (-1, -1,  0,  0);
  TEST_I ( 0,  0, -1, -1);

#ifdef DEBUG
  printf ("%d error%s\n", num_errors, (num_errors == 1) ? "" : "s");
#endif

  return 0;
};
