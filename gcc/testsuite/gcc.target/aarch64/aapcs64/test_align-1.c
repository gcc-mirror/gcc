/* Test AAPCS64 layout.

   Test the comformance to the alignment and padding requirements.

   B.4  If the argument type is a Composite Type then the size of the
        argument is rounded up to the nearest multiple of 8 bytes.
   C.4  If the argument is an HFA, a Quad-precision Floating-point or Short
	Vector Type then the NSAA is rounded up to the larger of 8 or the
	Natural Alignment of the argument's type.
   C.12 The NSAA is rounded up to the larger of 8 or the Natural Alignment
	of the argument's type.
   C.14 If the size of the argument is less than 8 bytes then the size of
	the argument is set ot 8 bytes.  The effect is as if the argument
	was copied to the least significant bits of a 64-bit register and
	the remaining bits filled with unspecified values.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_align-1.c"
#include "type-def.h"

struct y
{
  int p;
  int q;
  int r;
  int s;
};

struct y v1 = { 1, 2, 3, 4 };
struct y v2 = { 5, 6, 7, 8 };
struct y v3 = { 9, 10, 11, 12 };
struct y v4 = { 13, 14, 15, 16 };

struct z
{
  double x[4];
};

struct z a = { 5.0, 6.0, 7.0, 8.0 };
struct z b = { 9.0, 10.0, 11.0, 12.0 };

vf4_t c = { 13.f, 14.f, 15.f, 16.f };

struct x
{
  vf4_t v;
} w;

char ch='a';
short sh=13;
int i=14;
long long ll=15;

struct s1
{
  short sh[3];
} s1;

struct s2
{
  int i[2];
  char c;
} s2;

struct ldx2_t
{
  long double ld[2];
} ldx2 = { 12345.67890L, 23456.78901L };

union u_t
{
  long double ld;
  double d[2];
} u;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  w.v = (vf4_t){ 17.f, 18.f, 19.f, 20.f };
  s1.sh[0] = 16;
  s1.sh[1] = 17;
  s1.sh[2] = 18;
  s2.i[0] = 19;
  s2.i[1] = 20;
  s2.c = 21;
  u.ld = 34567.89012L;
}

#include "abitest.h"
#else

  ARG(struct y, v1, X0)
  ARG(struct y, v2, X2)
  ARG(struct y, v3, X4)
  ARG(struct y, v4, X6)
  ARG(struct z, a, D0)
  ARG(struct z, b, D4)
  ARG(double, 12.5, STACK)
  ARG(vf4_t, c, STACK+16)       /* [C.4] 16-byte aligned short vector */
  ARG(double, 17.0, STACK+32)
  ARG(struct x, w, STACK+48)    /* [C.12] 16-byte aligned small struct */
#ifndef __AAPCS64_BIG_ENDIAN__
  ARG(char, ch, STACK+64)       /* [C.14] char  padded to the size of 8 bytes */
  ARG(short, sh, STACK+72)      /* [C.14] short padded to the size of 8 bytes */
  ARG(int, i, STACK+80)         /* [C.14] int   padded to the size of 8 bytes */
#else
  ARG(char, ch, STACK+71)
  ARG(short, sh, STACK+78)
  ARG(int, i, STACK+84)
#endif
  ARG(long long, ll, STACK+88)
  ARG(struct s1, s1, STACK+96)  /* [B.4] small struct padded to the size of 8 bytes */
  ARG(double, 18.0, STACK+104)
  ARG(struct s2, s2, STACK+112) /* [B.4] small struct padded to the size of 16 bytes */
  ARG(double, 19.0, STACK+128)
  ARG(long double, 30.0L, STACK+144)  /* [C.4] 16-byte aligned quad-precision */
  ARG(double, 31.0, STACK+160)
  ARG(struct ldx2_t, ldx2, STACK+176) /* [C.4] 16-byte aligned HFA */
  ARG(double, 32.0, STACK+208)
  ARG(__int128, 33, STACK+224)  /* [C.12] 16-byte aligned 128-bit integer */
  ARG(double, 34.0, STACK+240)
  ARG(union u_t, u, STACK+256)  /* [C.12] 16-byte aligned small composite (union in this case) */
  LAST_ARG_NONFLAT (int, 35.0, STACK+272, i32in64)
#endif
