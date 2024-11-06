/* PR rtl-optimization/38245 */
/* { dg-do compile } */
/* { dg-options "-std=gnu17" } */

#include "pr38245-3.h"

int
b1 (long long a1, long a2, long a3, long a4,
    long a5, long a6, long a7, long a8,
    long long a9, long a10, long a11, long a12,
    long a13, long a14, long a15, long a16,
    long long a17, long a18, long a19, long a20,
    long a21, long a22, long a23, long a24,
    long long a25, long a26, long a27, long a28,
    long a29, long a30, long a31, long a32)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10
	 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20
	 + a21 + a22 + a23 + a24 + a25 + a26 + a27 + a28 + a29 + a30
	 + a31 + a32 + globv;
}

int
b2 (long long a1, long a2, long a3, long a4,
    long a5, long a6, long a7, long a8,
    long long a9, long a10, long a11, long a12,
    long a13, long a14, long a15, long a16,
    long long a17, long a18, long a19, long a20,
    long a21, long a22, long a23, long a24,
    long long a25, long a26, long a27, long a28,
    long a29, long a30, long a31, long a32)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10
	 + a11 + a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20
	 + a21 + a22 + a23 + a24 + a25 + a26 + a27 + a28 + a29 + a30
	 + a31 + a32;
}

int
b3 (long long a1, long a2, long a3, long a4,
    long a5, long a6, long a7, struct B a8,
    long long a9, struct B a10, long a11, struct B a12,
    long a13, long a14, long a15, long a16,
    long long a17, struct B a18, long a19, struct B a20,
    long a21, long a22, long a23, long a24,
    long long a25, struct B a26, long a27, struct B a28,
    long a29, long a30, long a31, long a32)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8.a + a9 + a10.a
	 + a11 + a12.a + a13 + a14 + a15 + a16 + a17 + a18.a + a19 + a20.a
	 + a21 + a22 + a23 + a24 + a25 + a26.a + a27 + a28.a + a29 + a30
	 + a31 + a32 + globv;
}

int
b4 (long long a1, long a2, long a3, long a4,
    long a5, long a6, long a7, struct B a8,
    long long a9, struct B a10, long a11, struct B a12,
    long a13, long a14, long a15, long a16,
    long long a17, struct B a18, long a19, struct B a20,
    long a21, long a22, long a23, long a24,
    long long a25, struct B a26, long a27, struct B a28,
    long a29, long a30, long a31, long a32)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8.a + a9 + a10.a
	 + a11 + a12.a + a13 + a14 + a15 + a16 + a17 + a18.a + a19 + a20.a
	 + a21 + a22 + a23 + a24 + a25 + a26.a + a27 + a28.a + a29 + a30
	 + a31 + a32;
}

int
b5 (double a1, int a2, int a3, int a4, int a5, int a6, double a7,
    double a8, double a9)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + globv;
}

int
b6 (double a1, int a2, int a3, int a4, int a5, int a6, double a7,
    double a8, double a9)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9;
}

int
b7 (int a1, int a2)
{
  return a1 + a2 + globv;
}

int
b8 (int a1, int a2)
{
  return a1 + a2;
}

int
b9 (int a1, int a2, int a3, int a4, int a5, int a6, int a7)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7 + globv;
}

int
b10 (int a1, int a2, int a3, int a4, int a5, int a6, int a7)
{
  return a1 + a2 + a3 + a4 + a5 + a6 + a7;
}
