/* { dg-do compile } */
/* { dg-options "-O2 -mlasx  -Werror -Wextra" } */

#include <lasxintrin.h>

__m256i v = {0, 0, 0, 0};

#define TEST(NAME)			  \
  void test_##NAME (void)		  \
  {					  \
    long long r[4];			  \
    __lasx_##NAME (v, r, 0);		  \
  }

TEST (xvst);
TEST (xvstx);

#define TEST1(NAME, TYPE, NUM)		  \
  void test_##NAME (void)		  \
  {					  \
    TYPE r[NUM];			  \
    __lasx_##NAME (v, r, 0, 0);		  \
  }

TEST1 (xvstelm_b, char, 32);
TEST1 (xvstelm_h, short, 16);
TEST1 (xvstelm_w, int, 8);
TEST1 (xvstelm_d, long long, 4);
