/* { dg-do compile } */
/* { dg-options "-O2 -mlsx  -Werror -Wextra" } */

#include <lsxintrin.h>

__m128i v = {0, 0};

#define TEST(NAME)			  \
  void test_##NAME (void)		  \
  {					  \
    long long r[2];			  \
    __lsx_##NAME (v, r, 0);		  \
  }

TEST (vst);
TEST (vstx);

#define TEST1(NAME, TYPE, NUM)		  \
  void test_##NAME (void)		  \
  {					  \
    TYPE r[NUM];			  \
    __lsx_##NAME (v, r, 0, 0);		  \
  }

TEST1 (vstelm_b, char, 16);
TEST1 (vstelm_h, short, 8);
TEST1 (vstelm_w, int, 4);
TEST1 (vstelm_d, long long, 2);
