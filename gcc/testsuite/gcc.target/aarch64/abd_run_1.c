/* { dg-do run } */
/* { dg-options "-O3" } */

#pragma GCC target "+nosve"
#define N 16

#define ABD_ABS
#include "abd.h"

TEST1(signed, int)
TEST1(signed, short)
TEST1(signed, char)

TEST2(signed, char, short)
TEST2(signed, short, int)
TEST2(signed, int, long)

TEST1(unsigned, int)
TEST1(unsigned, short)
TEST1(unsigned, char)

TEST2(unsigned, char, short)
TEST2(unsigned, short, int)

#define EMPTY { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
#define sA { -50, -50, -50, -50, -50, -50, -50, -50, -50, -50, -50, -50, -50, -50, -50, -50 }
#define uA { 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100 }
#define B { 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25 }
#define GOLD { 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75 }

typedef signed char    s8;
typedef unsigned char  u8;
typedef signed short   s16;
typedef unsigned short u16;
typedef signed int     s32;
typedef unsigned int   u32;
typedef signed long    s64;
typedef unsigned long  u64;

s8  sc_out[] = EMPTY;
u8  uc_out[] = EMPTY;
s16 ss_out[] = EMPTY;
u16 us_out[] = EMPTY;
s32 si_out[] = EMPTY;
u32 ui_out[] = EMPTY;
s64 sl_out[] = EMPTY;
u64 ul_out[] = EMPTY;

s8 sc_A[] = sA;
s8 sc_B[] = B;
u8 uc_A[] = uA;
u8 uc_B[] = B;

s16 ss_A[] = sA;
s16 ss_B[] = B;
u16 us_A[] = uA;
u16 us_B[] = B;

s32 si_A[] = sA;
s32 si_B[] = B;
u32 ui_A[] = uA;
u32 ui_B[] = B;

s8 sc_gold[] = GOLD;
u8 uc_gold[] = GOLD;
s16 ss_gold[] = GOLD;
u16 us_gold[] = GOLD;
s32 si_gold[] = GOLD;
u32 ui_gold[] = GOLD;
s64 sl_gold[] = GOLD;
u64 ul_gold[] = GOLD;

extern void abort (void);

#define CLEAR(arr)          \
for (int i = 0; i < N; i++) \
  arr[i] = 0;

#define COMPARE(A, B)       \
for (int i = 0; i < N; i++) \
  if (A[i] != B[i])         \
    abort();

int main ()
{
  fn_signed_char (sc_A, sc_B, sc_out);
  COMPARE (sc_out, sc_gold);

  fn_unsigned_char (uc_A, uc_B, uc_out);
  COMPARE (uc_out, uc_gold);

  fn_signed_short (ss_A, ss_B, ss_out);
  COMPARE (ss_out, ss_gold)

  fn_unsigned_short (us_A, us_B, us_out);
  COMPARE (us_out, us_gold)

  fn_signed_int (si_A, si_B, si_out);
  COMPARE (si_out, si_gold);

  fn_unsigned_int (ui_A, ui_B, ui_out);
  COMPARE (ui_out, ui_gold);


  fn_signed_char_char_short (sc_B, sc_A, ss_out);
  COMPARE(ss_gold, ss_out); 

  fn_signed_short_short_int (ss_A, ss_B, si_out);
  COMPARE(si_gold, si_out); 

  fn_signed_int_int_long (si_B, si_A, sl_out);
  COMPARE(sl_gold, sl_out);

  fn_unsigned_char_char_short (uc_B, uc_A, us_out);
  COMPARE(us_gold, us_out); 

  fn_unsigned_short_short_int (us_A, us_B, ui_out);
  COMPARE(ui_gold, ui_out); 

  return 0;
}

