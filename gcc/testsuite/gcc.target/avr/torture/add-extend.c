/* { dg-do run } */

typedef __UINT8_TYPE__ u8;
typedef __UINT16_TYPE__ u16;
__extension__ typedef __uint24 u24;
typedef __UINT32_TYPE__ u32;

typedef __INT8_TYPE__ s8;
typedef __INT16_TYPE__ s16;
__extension__ typedef __int24 s24;
typedef __INT32_TYPE__ s32;

#define NI __attribute__((noipa))

NI u32 addu_32_8  (u32 a, u8 b)  { return a + b; }
NI u32 addu_32_16 (u32 a, u16 b) { return a + b; }
NI u32 addu_32_24 (u32 a, u24 b) { return a + b; }

NI u24 addu_24_8  (u24 a, u8 b)  { return a + b; }
NI u24 addu_24_16 (u24 a, u16 b) { return a + b; }

NI u16 addu_16_8  (u16 a, u8 b)  { return a + b; }

/************************/

NI s32 adds_32_8  (s32 a, s8 b)  { return a + b; }
NI s32 adds_32_16 (s32 a, s16 b) { return a + b; }
NI s32 adds_32_24 (s32 a, s24 b) { return a + b; }

NI s24 adds_24_8  (s24 a, s8 b)  { return a + b; }
NI s24 adds_24_16 (s24 a, s16 b) { return a + b; }

NI s16 adds_16_8  (s16 a, s8 b)  { return a + b; }

/************************/

NI u32 addu_32 (u32 a, u32 b)    { return a + b; }
NI u24 addu_24 (u24 a, u24 b)    { return a + b; }
NI u16 addu_16 (u16 a, u16 b)    { return a + b; }

NI s32 adds_32 (s32 a, s32 b)    { return a + b; }
NI s24 adds_24 (s24 a, s24 b)    { return a + b; }
NI s16 adds_16 (s16 a, s16 b)    { return a + b; }

/************************/

NI u8 next (void *p0, u8 n_bytes)
{
  u8 *p = (u8*) p0;

  u8 n;
  for (n = 0; n < n_bytes; ++n)
    {
      u8 val = *p;

      /* Cycle over 0 -> 1 -> -1 -> 0 */
      if (++val == 2)
	val = -1;

      *p++ = val;
      if (val)
	return 1;
    }

  return 0;
}

#define MK_TEST(A, B)							\
NI void test_##A##_##B (void)						\
{									\
  u##A a = 0;								\
  do									\
    {									\
      u##B b = 0;							\
      do								\
	{								\
	  if (addu_##A##_##B (a, b) != addu_##A (a, b))			\
	    __builtin_exit (11);					\
	  								\
	  if (adds_##A##_##B (a, b) != adds_##A ((s##A) a, (s##B) b))	\
	    __builtin_exit (13);					\
	  								\
	} while (next (&b, sizeof (b)));				\
    } while (next (&a, sizeof (a)));					\
}

MK_TEST (16, 8)

MK_TEST (24, 8)
MK_TEST (24, 16)

MK_TEST (32, 8)
MK_TEST (32, 16)
MK_TEST (32, 24)


int main (void)
{
  test_16_8 ();

  test_24_8 ();
  test_24_16 ();

  test_32_8 ();
  test_32_16 ();
  test_32_24 ();

  return 0;
}
