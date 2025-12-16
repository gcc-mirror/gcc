/* { dg-do run } */
/* { dg-options "-mgfni -mavx512vl -mavx512bw -mavx512f -O3 -Wno-shift-count-negative -march=x86-64 -mtune=generic" } */
/* { dg-skip-if "Assembler support missing" { *-*-solaris2.* && { ! gas } } } */

#include <string.h>

#ifndef N1
#define N1 5
#endif

#ifndef N2
#define N2 3
#endif

#ifndef N3
#define N3 1
#endif

#ifndef N4
#define N4 7
#endif

#ifndef N5
#define N5 -3
#endif

#ifndef FILLER
#define FILLER 0xab
#endif

#define FUNC(N)							\
  void ubyteshiftl##N(unsigned char *a, int len)		\
  {								\
    int i;							\
    for (i = 0; i < len; i++)					\
      a[i] <<= N;						\
  }								\
								\
  void ubyteshiftr##N(unsigned char *a, int len)		\
  {								\
    int i;							\
    for (i = 0; i < len; i++)					\
      a[i] >>= N;						\
  }								\
								\
  void ubyteshiftl_mask##N(unsigned char *a, int len)		\
  {								\
    int i;							\
    for (i = 0; i < len; i++)					\
      if (a[i] & 1)						\
	a[i] <<= N;						\
  }								\
								\
  void sbyteshiftl##N(signed char *a, int len)			\
  {								\
    int i;							\
    for (i = 0; i < len; i++)					\
      a[i] <<= N;						\
  }								\
								\
  void sbyteshiftr##N(signed char *a, int len)			\
  {								\
    int i;							\
    for (i = 0; i < len; i++)					\
      a[i] >>= N;						\
  }								\
								\
  void ubyteror##N(unsigned char *a, int len)			\
  {								\
    int i;							\
    for (i = 0; i < len; i++)					\
      a[i] = a[i] << N | a[i] >> (8-N);				\
  }								\
								\
  void ubyterol##N(unsigned char *a, int len)			\
  {								\
    int i;							\
    for (i = 0; i < len; i++)					\
      a[i] = a[i] >> N | a[i] << (8-N);				\
  }								\
  void ubyteshiftl##N##ref(unsigned char *a, int len)		\
  {								\
    int i;							\
    _Pragma("GCC novector")					\
      for (i = 0; i < len; i++)					\
	a[i] <<= N;						\
  }								\
								\
  void ubyteshiftr##N##ref(unsigned char *a, int len)		\
  {								\
    int i;							\
    _Pragma("GCC novector")					\
      for (i = 0; i < len; i++)					\
	a[i] >>= N;						\
  }								\
								\
  void ubyteshiftl_mask##N##ref(unsigned char *a, int len)	\
  {								\
    int i;							\
    _Pragma("GCC novector")					\
      for (i = 0; i < len; i++)					\
	if (a[i] & 1)						\
	  a[i] <<= N;						\
  }								\
								\
  void sbyteshiftl##N##ref(signed char *a, int len)		\
  {								\
    int i;							\
    _Pragma("GCC novector")					\
      for (i = 0; i < len; i++)					\
	a[i] <<= N;						\
  }								\
								\
  void sbyteshiftr##N##ref(signed char *a, int len)		\
  {								\
    int i;							\
    _Pragma("GCC novector")					\
      for (i = 0; i < len; i++)					\
	a[i] >>= N;						\
  }								\
								\
  void ubyteror##N##ref(unsigned char *a, int len)		\
  {								\
    int i;							\
    _Pragma("GCC novector")					\
      for (i = 0; i < len; i++)					\
	a[i] = a[i] << N | a[i] >> (8-N);			\
  }								\
								\
  void ubyterol##N##ref(unsigned char *a, int len)		\
  {								\
    int i;							\
    _Pragma("GCC novector")					\
      for (i = 0; i < len; i++)					\
	a[i] = a[i] >> N | a[i] << (8-N);			\
  }

FUNC (N1)
FUNC (N2)
FUNC (N3)
FUNC (N4)
FUNC (N5)

#define TEST(N, func)					\
  memset (array, filler, len);				\
  func##N (array, len);					\
  memset (array2, filler, len);				\
  func##N##ref (array2, len);				\
  if (memcmp (array, array2, len)) __builtin_abort ()

int main ()
{
  __builtin_cpu_init ();
  if (!__builtin_cpu_supports ("gfni"))
    return 0;

  const unsigned long len = 256;
  char array[len], array2[len];
  unsigned char filler = FILLER;

  TEST (N1, ubyteshiftl);
  TEST (N1, ubyteshiftl_mask);
  TEST (N1, sbyteshiftl);
  TEST (N1, sbyteshiftr);
  TEST (N1, ubyteror);
  TEST (N1, ubyterol);

  TEST (N2, ubyteshiftl);
  TEST (N2, ubyteshiftl_mask);
  TEST (N2, sbyteshiftl);
  TEST (N2, sbyteshiftr);
  TEST (N2, ubyteror);
  TEST (N2, ubyterol);

  TEST (N3, ubyteshiftl);
  TEST (N3, ubyteshiftl_mask);
  TEST (N3, sbyteshiftl);
  TEST (N3, sbyteshiftr);
  TEST (N3, ubyteror);
  TEST (N3, ubyterol);

  TEST (N4, ubyteshiftl);
  TEST (N4, ubyteshiftl_mask);
  TEST (N4, sbyteshiftl);
  TEST (N4, sbyteshiftr);
  TEST (N4, ubyteror);
  TEST (N4, ubyterol);

  TEST (N5, ubyteshiftl);
  TEST (N5, ubyteshiftl_mask);
  TEST (N5, sbyteshiftl);
  TEST (N5, sbyteshiftr);
  TEST (N5, ubyteror);
  TEST (N5, ubyterol);

  return 0;
}
