/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -ftree-vectorize" } */

#ifndef ALIGN16
#define ALIGN16 __attribute__((__aligned__(32)))
#endif

#define I16_B0	1
#define I16_B1	23
#define I16_B2	345
#define I16_B3	6789
#define I16_B4	1011
#define I16_B5	-12
#define I16_B6	1314
#define I16_B7	-1516
#define I16_B8	-1516
#define I16_B9	1314
#define I16_B10	-12
#define I16_B11	1011
#define I16_B12	6789
#define I16_B13	345
#define I16_B14	23
#define I16_B15	1

#define I16_C0	2
#define I16_C1	31567
#define I16_C2	1112
#define I16_C3	13
#define I16_C4	14
#define I16_C5	15
#define I16_C6	-16
#define I16_C7	17
#define I16_C8	2
#define I16_C9	31567
#define I16_C10	1112
#define I16_C11	13
#define I16_C12	14
#define I16_C13	15
#define I16_C14	-16
#define I16_C15	17

union ui2 {
  int		si[16];
  unsigned int	ui[16];
};

union us {
  short		 ss[16];
  unsigned short us[16];
};

static union ui2 i32_sa ALIGN16;
static union ui2 i32_ua ALIGN16;

static union us i16_b ALIGN16 = {
  (short)I16_B0,
  (short)I16_B1,
  (short)I16_B2,
  (short)I16_B3,
  (short)I16_B4,
  (short)I16_B5,
  (short)I16_B6,
  (short)I16_B7,
  (short)I16_B8,
  (short)I16_B9,
  (short)I16_B10,
  (short)I16_B11,
  (short)I16_B12,
  (short)I16_B13,
  (short)I16_B14,
  (short)I16_B15,
};

static union us i16_c ALIGN16 = {
  (short)I16_C0,
  (short)I16_C1,
  (short)I16_C2,
  (short)I16_C3,
  (short)I16_C4,
  (short)I16_C5,
  (short)I16_C6,
  (short)I16_C7,
  (short)I16_C8,
  (short)I16_C9,
  (short)I16_C10,
  (short)I16_C11,
  (short)I16_C12,
  (short)I16_C13,
  (short)I16_C14,
  (short)I16_C15,
};

static long i32_sr[16] ALIGN16 = {
  ((int)I16_B0)  * ((int)I16_C0),
  ((int)I16_B1)  * ((int)I16_C1),
  ((int)I16_B2)  * ((int)I16_C2),
  ((int)I16_B3)  * ((int)I16_C3),
  ((int)I16_B4)  * ((int)I16_C4),
  ((int)I16_B5)  * ((int)I16_C5),
  ((int)I16_B6)  * ((int)I16_C6),
  ((int)I16_B7)  * ((int)I16_C7),
  ((int)I16_B8)  * ((int)I16_C8),
  ((int)I16_B9)  * ((int)I16_C9),
  ((int)I16_B10) * ((int)I16_C10),
  ((int)I16_B11) * ((int)I16_C11),
  ((int)I16_B12) * ((int)I16_C12),
  ((int)I16_B13) * ((int)I16_C13),
  ((int)I16_B14) * ((int)I16_C14),
  ((int)I16_B15) * ((int)I16_C15),
};

static unsigned long i32_ur[16] ALIGN16 = {
  ((unsigned int)(unsigned short)I16_B0)  * ((unsigned int)(unsigned short)I16_C0),
  ((unsigned int)(unsigned short)I16_B1)  * ((unsigned int)(unsigned short)I16_C1),
  ((unsigned int)(unsigned short)I16_B2)  * ((unsigned int)(unsigned short)I16_C2),
  ((unsigned int)(unsigned short)I16_B3)  * ((unsigned int)(unsigned short)I16_C3),
  ((unsigned int)(unsigned short)I16_B4)  * ((unsigned int)(unsigned short)I16_C4),
  ((unsigned int)(unsigned short)I16_B5)  * ((unsigned int)(unsigned short)I16_C5),
  ((unsigned int)(unsigned short)I16_B6)  * ((unsigned int)(unsigned short)I16_C6),
  ((unsigned int)(unsigned short)I16_B7)  * ((unsigned int)(unsigned short)I16_C7),
  ((unsigned int)(unsigned short)I16_B8)  * ((unsigned int)(unsigned short)I16_C8),
  ((unsigned int)(unsigned short)I16_B9)  * ((unsigned int)(unsigned short)I16_C9),
  ((unsigned int)(unsigned short)I16_B10) * ((unsigned int)(unsigned short)I16_C10),
  ((unsigned int)(unsigned short)I16_B11) * ((unsigned int)(unsigned short)I16_C11),
  ((unsigned int)(unsigned short)I16_B12) * ((unsigned int)(unsigned short)I16_C12),
  ((unsigned int)(unsigned short)I16_B13) * ((unsigned int)(unsigned short)I16_C13),
  ((unsigned int)(unsigned short)I16_B14) * ((unsigned int)(unsigned short)I16_C14),
  ((unsigned int)(unsigned short)I16_B15) * ((unsigned int)(unsigned short)I16_C15),
};

#ifndef ALIGN32
#define ALIGN32 __attribute__((__aligned__(32)))
#endif

#define I32_B0	1
#define I32_B1	23
#define I32_B2	345
#define I32_B3	6789
#define I32_B4	101112
#define I32_B5	-13
#define I32_B6	141516
#define I32_B7	-1718

#define I32_C0	2
#define I32_C1	45678910
#define I32_C2	1112
#define I32_C3	13
#define I32_C4	14
#define I32_C5	15
#define I32_C6	-16
#define I32_C7	17

union ul {
  long		sl[8];
  unsigned long	ul[8];
};

union ui {
  int		si[8];
  unsigned int	ui[8];
};

static union ul i64_sa ALIGN32;
static union ul i64_ua ALIGN32;

static union ui i32_b ALIGN32 = {
  (int)I32_B0,
  (int)I32_B1,
  (int)I32_B2,
  (int)I32_B3,
  (int)I32_B4,
  (int)I32_B5,
  (int)I32_B6,
  (int)I32_B7,
};

static union ui i32_c ALIGN32 = {
  (int)I32_C0,
  (int)I32_C1,
  (int)I32_C2,
  (int)I32_C3,
  (int)I32_C4,
  (int)I32_C5,
  (int)I32_C6,
  (int)I32_C7,
};

static long i64_sr[8] ALIGN32 = {
  ((long)I32_B0) * ((long)I32_C0),
  ((long)I32_B1) * ((long)I32_C1),
  ((long)I32_B2) * ((long)I32_C2),
  ((long)I32_B3) * ((long)I32_C3),
  ((long)I32_B4) * ((long)I32_C4),
  ((long)I32_B5) * ((long)I32_C5),
  ((long)I32_B6) * ((long)I32_C6),
  ((long)I32_B7) * ((long)I32_C7),
};

static unsigned long i64_ur[8] ALIGN32 = {
  ((unsigned long)(unsigned)I32_B0) * ((unsigned long)(unsigned)I32_C0),
  ((unsigned long)(unsigned)I32_B1) * ((unsigned long)(unsigned)I32_C1),
  ((unsigned long)(unsigned)I32_B2) * ((unsigned long)(unsigned)I32_C2),
  ((unsigned long)(unsigned)I32_B3) * ((unsigned long)(unsigned)I32_C3),
  ((unsigned long)(unsigned)I32_B4) * ((unsigned long)(unsigned)I32_C4),
  ((unsigned long)(unsigned)I32_B5) * ((unsigned long)(unsigned)I32_C5),
  ((unsigned long)(unsigned)I32_B6) * ((unsigned long)(unsigned)I32_C6),
  ((unsigned long)(unsigned)I32_B7) * ((unsigned long)(unsigned)I32_C7),
};


int main ()
{
  int i;

  /* Signed 16x16 -> 32-bit tests */
  for (i = 0; i < 16; i++)
    i32_sa.si[i] = ((long)i16_b.ss[i]) * ((long)i16_c.ss[i]);

  for (i = 0; i < 16; i++)
    if (i32_sa.si[i] != i32_sr[i])
      __builtin_abort ();

  /* Unsigned 16x16 -> 32-bit tests */
  for (i = 0; i < 16; i++)
    i32_ua.ui[i] = ((long)i16_b.us[i]) * ((long)i16_c.us[i]);

  for (i = 0; i < 16; i++)
    if (i32_ua.ui[i] != i32_ur[i])
      __builtin_abort ();

  /* Signed 32x32 -> 64-bit tests */
  for (i = 0; i < 8; i++)
    i64_sa.sl[i] = ((long)i32_b.si[i]) * ((long)i32_c.si[i]);

  for (i = 0; i < 8; i++)
    if (i64_sa.sl[i] != i64_sr[i])
      __builtin_abort ();

  /* Unsigned 32x32 -> 64-bit tests */
  for (i = 0; i < 8; i++)
    i64_ua.ul[i] = ((long)i32_b.ui[i]) * ((long)i32_c.ui[i]);

  for (i = 0; i < 8; i++)
    if (i64_ua.ul[i] != i64_ur[i])
      __builtin_abort ();

  return 0;
}
