/* { dg-do run { target int128 } } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "" { target c++ } } */

#ifndef __cplusplus
extern void abort (void);
#else
extern "C" void abort (void);
#endif

#define MK_CONST128(A,B,C,D) \
        ( (((unsigned __int128) (unsigned int) A) << 96) \
         | (((unsigned __int128) (unsigned int) B) << 64) \
         | (((unsigned __int128) (unsigned int) C) << 32) \
         | ((unsigned __int128) (unsigned int) D) )

#define MK_CONST128_SIGNED(A,B,C,D) \
        ((__int128) MK_CONST128(A, B, C, D))

#define MINUS_2 MK_CONST128_SIGNED (0xffffffffu, 0xffffffffu, 0xffffffffu, \
		0xfffffffeu)
#define MINUS_3 MK_CONST128_SIGNED (0xffffffffu, 0xffffffffu, 0xffffffffu, \
		0xfffffffdu)
#define MINUS_6 MK_CONST128_SIGNED (0xffffffffu, 0xffffffffu, 0xffffffffu, \
		0xfffffffau)
#define PLUS_1	MK_CONST128_SIGNED (0, 0, 0, 1)
#define PLUS_2	MK_CONST128_SIGNED (0, 0, 0, 2)
#define PLUS_3	MK_CONST128_SIGNED (0, 0, 0, 3)
#define PLUS_6	MK_CONST128_SIGNED (0, 0, 0, 6)
#define PLUS_10	MK_CONST128_SIGNED (0, 0, 0, 10)

#define U_8	MK_CONST128 (0, 0, 0, 8)
#define U_MAX	MK_CONST128 (0xffffffff,0xffffffff,0xffffffff,0xffffffff)
#define U_CST1	MK_CONST128 (0xbeeffeed, 0xdeafcafe, 0xaffefade, 0x12345678)
#define U_CST2	MK_CONST128 (0x41100112, 0x21503501, 0x50010521, 0xedcba987)

signed __int128 foo_neg (signed __int128 v)
{
  return -v;
}

unsigned __int128 foo_xor (unsigned __int128 x, unsigned __int128 y)
{
  return x ^ y;
}

unsigned __int128 foo_inv (unsigned __int128 v)
{
  return ~v;
}

unsigned __int128 foo_rotate_left (unsigned __int128 v)
{
  unsigned __int128 c;
  int i;
  for (i = 0; i < 128; i++)
    {
      c = v >> 127;
      v <<= 1;
      v |= c;
    }
  return v;
}

unsigned __int128 foo_rotate_right (unsigned __int128 v)
{
  unsigned __int128 c;
  int i;
  for (i = 0; i < 128; i++)
    {
      c = (v & ((unsigned __int128) 1)) << 127;
      v >>= 1;
      v |= c;
    }
  return v;
}

void foo_swap (unsigned __int128 *x, unsigned __int128 *y)
{
  unsigned __int128 x1 = x[0];
  unsigned __int128 y1 = y[0];
  x1 ^= y1 ^= x1 ^= y1;
  x[0] = x1;
  y[0] = y1;
}

__int128 foo_add (signed __int128 a, unsigned __int128 b)
{
  return (__int128) (a + (__int128) b);
}

__int128 foo_sub (unsigned __int128 a, signed __int128 b)
{
  return (__int128) ((__int128) a - b);
}

__int128 foo_mul (signed __int128 a, signed __int128 b)
{
  return a * b;
}

__int128 foo_div (signed __int128 a, signed __int128 b)
{
  return a / b;
}

__int128 foo_shl (signed __int128 a, int shift)
{
  return a << (shift & 127);
}

__int128 foo_shr (signed __int128 a, int shift)
{
  return a >> (shift & 127);
}

int main(void)
{
  __int128 rslt;
  unsigned __int128 u1, u2;

  rslt = foo_add (MINUS_2, U_8);
  if (rslt != PLUS_6)
    abort ();
  rslt = foo_sub (U_8, MINUS_2);
  if (rslt != PLUS_10)
     abort ();
  rslt = foo_sub ((unsigned __int128) foo_mul (MINUS_2, MINUS_2), MINUS_2);
  if (rslt != PLUS_6)
    abort ();
  if (rslt != foo_shl (PLUS_3, 1))
    abort ();
  rslt = foo_shl (MINUS_3, 1);
  if (rslt != MINUS_6)
    abort ();
  if (foo_shr (MINUS_6, 1) != MINUS_3)
    abort ();
  if (foo_div (MINUS_6, MINUS_3) != PLUS_2)
    abort ();
  if (foo_rotate_left (U_CST1) != U_CST1)
    abort ();
  if (foo_rotate_right (U_CST1) != U_CST1)
    abort ();
  u1 = U_CST1;
  u2 = U_8;
  foo_swap (&u1, &u2);
  if (u1 != U_8 || u2 != U_CST1)
    abort ();

  if (foo_inv (U_CST2) != U_CST1)
    abort ();
  if (foo_neg (PLUS_2) != MINUS_2)
    abort ();
  if (foo_neg ((signed __int128) U_CST1) != foo_add (PLUS_1, foo_xor (U_CST1, U_MAX)))
    abort ();
  return 0;
}
