/* PR tree-optimization/91183 - strlen of a strcpy result with a conditional
   source not folded
   Test to verify that strlen can determine string lengths from wider stores
   than narrow characters.  This matters because on targets that can handle
   unaligned stores and where GCC lowers multi-character stores into smaller
   numbers of wider stores.
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized" }  */

#include "strlenopt.h"

#define CHAR_BIT __CHAR_BIT__

typedef __INT16_TYPE__  int16_t;
typedef __INT32_TYPE__  int32_t;
typedef __INT64_TYPE__  int64_t;
typedef __UINT64_TYPE__ uint64_t;

#define CAT(x, y) x ## y
#define CONCAT(x, y) CAT (x, y)
#define FAILNAME(name) CONCAT (call_ ## name ##_on_line_, __LINE__)

#define FAIL(name) do {				\
    extern void FAILNAME (name) (void);		\
    FAILNAME (name)();				\
  } while (0)

/* Macros to emit a call to function named
     call_failed_to_be_eliminated_on_line_NNN()
   for each call that's expected to be eliminated.  The dg-final
   scan-tree-dump-time directive at the bottom of the test verifies
   that no such call appears in output.  */
#define ELIM(expr)					\
  if ((expr)) FAIL (not_eliminated); else (void)0

/* Verify that 'strlen (A) EXPECT' is folded to true.  When non-null,
   the first sizeof (INIT) - 1 bytes of the INIT arrray are stored
   in A first, followed by *(TYPE*)A = ASSIGN.  */
#define T(init, type, off, assign, expect) do {			\
    char a[32];							\
    memcpy (a, init ? init : "", init ? sizeof init - 1 : 0);	\
    *(type*)(a + off) = assign;					\
    ELIM (!(strlen (a) expect));				\
  } while (0)

/* Same as above but the assignment consisting of the two quadwords
   QW1 and QW2 to support int128_t.  */
#define T2(init, type, off, qw0, qw1, expect) do {			\
    char a[32];								\
    memcpy (a, init ? init : "", init ? sizeof init - 1: 0);		\
    type assign = ((type)qw0 << (sizeof (type) * CHAR_BIT / 2)) | (type)qw1; \
    *(type*)(a + off) = assign;						\
    ELIM (!(strlen (a) expect));					\
  } while (0)

/* Same as T but works around the optimizer dropping the initializing
   store before the assignment and defeating the strlen optimization.  */
#define TX(init, type, off, assign, expect) do {		\
    char a[32];							\
    strcpy (a, init + 2);					\
    strcat (a, init + sizeof (init) - 3);			\
    *(type*)(a + off) = assign;					\
    ELIM (!(strlen (a) expect));				\
  } while (0)


#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#  define I16(s) ((s[0] << 8) + s[1])
#  define I32(s) ((s[0] << 24) + (s[1] << 16) + (s[2] << 8) + s[3])
#  define I64(s)						\
  (((uint64_t)s[0] << 56)					\
   + ((uint64_t)s[1] << 48)					\
   + ((uint64_t)s[2] << 40)					\
   + ((uint64_t)s[3] << 32)					\
   + ((uint64_t)s[4] << 24)					\
   + ((uint64_t)s[5] << 16)					\
   + ((uint64_t)s[6] << 8)					\
   + s[7])
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#  define I16(s) ((s[1] << 8) + s[0])
#  define I32(s) ((s[3] << 24) + (s[2] << 16) + (s[1] << 8) + s[0])
#  define I64(s)						\
  (((uint64_t)s[7] << 56)					\
   + ((uint64_t)s[6] << 48)					\
   + ((uint64_t)s[5] << 40)					\
   + ((uint64_t)s[4] << 32)					\
   + ((uint64_t)s[3] << 24)					\
   + ((uint64_t)s[2] << 16)					\
   + ((uint64_t)s[1] << 8)					\
   + s[0])
#endif

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__

void store_16bit_be (void)
{
  T ("xxx", int16_t, 0, 0x0001, == 0);
  T ("xxx", int16_t, 0, 0x0010, == 0);
  T ("xxx", int16_t, 0, 0x0011, == 0);
  T ("xxx", int16_t, 0, 0x0100, == 1);
  T ("xxx", int16_t, 0, 0x1000, == 1);
  T ("xxx", int16_t, 0, 0x1100, == 1);
}

#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__

void store_16bit_le (int i)
{
  int16_t x0000 = I16 ("\0\0");
  int16_t x0001 = 0x0001;
  int16_t x0010 = 0x0010;
  int16_t x0011 = 0x0011;
  int16_t x0100 = 0x0100;
  int16_t x1000 = 0x1000;
  int16_t x1100 = 0x1100;

  T (0,        int16_t, 0, x0000, == 0);
  T ("x",      int16_t, 0, x0000, == 0);
  T ("xx",     int16_t, 0, x0000, == 0);
  T ("xxxx",   int16_t, 0, x0000, == 0);
  T (0,        int16_t, 0, x0001, == 1);
  T ("\0\0\0", int16_t, 0, x0001, == 1);
  T (0,        int16_t, 0, x0010, == 1);
  T ("x\0\0",  int16_t, 0, x0010, == 1);
  T (0,        int16_t, 0, x0011, == 1);
  T ("xx\0",   int16_t, 0, x0011, == 1);
  T (0,        int16_t, 0, x0100, == 0);
  T ("\0\0\0", int16_t, 0, x0100, == 0);
  T (0,        int16_t, 0, x1000, == 0);
  T ("x\0\0",  int16_t, 0, x1000, == 0);
  T (0,        int16_t, 0, x1100, == 0);
  T ("xx\0",   int16_t, 0, x1100, == 0);

  // FIXME: This fails because of the next test but succeeds on its own.
  // T (0,        int16_t, 0, i ? x0001 : x0010, == 1);
  T ("xxx",    int16_t, 0, i ? x0100 : x1100, == 0);
}

#endif

void store_32bit (volatile int i)
{
  T (0,      int32_t, 0, 0, == 0);
  T ("x",    int32_t, 0, 0, == 0);
  T ("xx",   int32_t, 0, 0, == 0);
  T ("xxx",  int32_t, 0, 0, == 0);
  T ("xxxx", int32_t, 0, 0, == 0);

  T ("\0",   int32_t, 1, 0, == 0);
  T ("x",    int32_t, 1, 0, == 1);
  T ("xx",   int32_t, 2, 0, == 2);
  T ("xxx",  int32_t, 3, 0, == 3);

  T ("xxx",  int32_t, 0, I32 ("\01\0\0\0"), == 1);
  T ("xxx",  int32_t, 0, I32 ("\0\01\0\0"), == 0);
  T ("xxx",  int32_t, 0, I32 ("\0\0\01\0"), == 0);
  T ("xxx",  int32_t, 0, I32 ("\0\0\0\01"), == 0);

  T ("xxx",  int32_t, 0, I32 ("\1\2\0\0"), == 2);
  T ("xxx",  int32_t, 0, I32 ("\0\1\2\0"), == 0);
  T ("xxx",  int32_t, 0, I32 ("\0\0\1\2"), == 0);

  T ("xxx",  int32_t, 0, I32 ("\1\2\3\0"), == 3);
  T ("xxx",  int32_t, 0, I32 ("\0\1\2\3"), == 0);

  int32_t x00332211 = I32 ("123\0");
  int32_t x00002211 = I32 ("12\0\0");
  int32_t x00000011 = I32 ("1\0\0\0");

  T ("xxxx", int32_t, 0, i ? x00332211 : x00002211, <= 3);
  T ("xxxx", int32_t, 0, i ? x00332211 : x00002211, >= 2);
  T ("xxxx", int32_t, 0, i ? x00332211 : x00000011, <= 3);
  T ("xxxx", int32_t, 0, i ? x00332211 : x00000011, >= 1);

  TX ("abcde",  int32_t, 0, i ? I32 ("1234") : I32 ("1235"), == 5);
  TX ("abcde",  int32_t, 1, i ? I32 ("1234") : I32 ("1235"), == 5);

  TX ("abcdef", int32_t, 0, i ? I32 ("1235") : I32 ("1234"), == 6);
  TX ("abcdef", int32_t, 1, i ? I32 ("1235") : I32 ("1234"), == 6);
  TX ("abcdef", int32_t, 2, i ? I32 ("1235") : I32 ("1234"), == 6);
  TX ("abcdef", int32_t, 3, i ? I32 ("124\0") : I32 ("123\0"), == 6);
  TX ("abcdef", int32_t, 3, i ? I32 ("12\0\0") : I32 ("13\0\0"), == 5);

  TX ("abcdef", int32_t, 3, i ? I32 ("12\0\0") : I32 ("123\0"), >= 5);
  TX ("abcdef", int32_t, 3, i ? I32 ("12\0\0") : I32 ("123\0"), < 7);
}

void store_64bit (int i)
{
  T2 ("xxxxxxx", int64_t, 0,                0, I32 ("\1\0\0\0"), == 1);
  T2 ("xxxxxxx", int64_t, 0,                0, I32 ("\0\1\0\0"), == 0);
  T2 ("xxxxxxx", int64_t, 0,                0, I32 ("\0\0\1\0"), == 0);
  T2 ("xxxxxxx", int64_t, 0,                0, I32 ("\0\00\0\1"), == 0);
  T2 ("xxxxxxx", int64_t, 0, I32 ("\1\0\0\0"), 0, == 0);
  T2 ("xxxxxxx", int64_t, 0, I32 ("\0\1\0\0"), 0, == 0);
  T2 ("xxxxxxx", int64_t, 0, I32 ("\0\0\1\0"), 0, == 0);
  T2 ("xxxxxxx", int64_t, 0, I32 ("\0\0\0\1"), 0, == 0);

  T2 ("xxxxxxx", int64_t, 0, 0, I32 ("\1\2\0\0"), == 2);
  T2 ("xxxxxxx", int64_t, 0, 0, I32 ("\0\1\2\0"), == 0);
  T2 ("xxxxxxx", int64_t, 0, 0, I32 ("\0\0\1\2"), == 0);

  T2 ("xxxxxxx", int64_t, 0, 0, I32 ("\1\2\3\0"), == 3);
  T2 ("xxxxxxx", int64_t, 0, 0, I32 ("\0\1\2\3"), == 0);

  T2 ("xxxxxxx", int64_t, 0, 0, I32 ("\1\2\3\4"), == 4);
  T2 ("xxxxxxx", int64_t, 0, I32 ("\5\0\0\0"), I32 ("\1\2\3\4"), == 5);
  T2 ("xxxxxxx", int64_t, 0, I32 ("\5\6\0\0"), I32 ("\1\2\3\4"), == 6);
  T2 ("xxxxxxx", int64_t, 0, I32 ("\5\6\7\0"), I32 ("\1\2\3\4"), == 7);

  int64_t x7777777 = I64 ("\7\7\7\7\7\7\7");
  int64_t x666666 = I64 ("\6\6\6\6\6\6\0");
  int64_t x4444 = I64 ("\4\4\4\4\0\0\0");
  int64_t x3333 = I64 ("\3\3\3\3\0\0\0");
  int64_t x1 = I64 ("\1\0\0\0\0\0\0");

  T ("x\0xxxxxx", int64_t, 0, i ? x7777777 : x666666, <= 7);
  T ("xx\0xxxxx", int64_t, 0, i ? x7777777 : x666666, >= 6);
  T ("xxx\0xxxx", int64_t, 0, i ? x666666 : x1, <= 6);
  T ("xxxx\0xxx", int64_t, 0, i ? x666666 : x1, >= 1);
  T ("xxxxxx\0x", int64_t, 0, i ? x4444 : x3333, == 4);
}

#ifdef __uint128_t

typedef __uint128_t     uint128_t;

void store_128bit (void)
{
  uint64_t x1 = I64 ("\1\0\0\0\0\0\0\0");
  uint64_t x01 = I64 ("\0\1\0\0\0\0\0\0");
  uint64_t x001 = I64 ("\0\0\1\0\0\0\0\0");
  uint64_t x0001 = I64 ("\0\0\0\1\0\0\0\0");
  uint64_t x00001 = I64 ("\0\0\0\0\1\0\0\0");
  uint64_t x000001 = I64 ("\0\0\0\0\0\1\0\0");
  uint64_t x0000001 = I64 ("\0\0\0\0\0\0\1\0");
  uint64_t x00000001 = I64 ("\0\0\0\0\0\0\0\1");

  T2 ("xxxxxxx", uint128_t, 0, 0,        x1, == 1);
  T2 ("xxxxxxx", uint128_t, 0, 0,       x01, == 0);
  T2 ("xxxxxxx", uint128_t, 0, 0,      x001, == 0);
  T2 ("xxxxxxx", uint128_t, 0, 0,     x0001, == 0);
  T2 ("xxxxxxx", uint128_t, 0, 0,    x00001, == 0);
  T2 ("xxxxxxx", uint128_t, 0, 0,   x000001, == 0);
  T2 ("xxxxxxx", uint128_t, 0, 0,  x0000001, == 0);
  T2 ("xxxxxxx", uint128_t, 0, 0, x00000001, == 0);

  T2 ("xxxxxxx", uint128_t, 0,        x1, 0, == 0);
  T2 ("xxxxxxx", uint128_t, 0,       x01, 0, == 0);
  T2 ("xxxxxxx", uint128_t, 0,      x001, 0, == 0);
  T2 ("xxxxxxx", uint128_t, 0,     x0001, 0, == 0);
  T2 ("xxxxxxx", uint128_t, 0,    x00001, 0, == 0);
  T2 ("xxxxxxx", uint128_t, 0,   x000001, 0, == 0);
  T2 ("xxxxxxx", uint128_t, 0,  x0000001, 0, == 0);
  T2 ("xxxxxxx", uint128_t, 0, x00000001, 0, == 0);

  T2 ("xxxxxxx", uint128_t, 0, 0, I64 ("\2\1\0\0\0\0\0\0"), == 2);
  T2 ("xxxxxxx", uint128_t, 0, 0, I64 ("\0\2\1\0\0\0\0\0"), == 0);

  T2 ("xxxxxxx", uint128_t, 0, 0, I64 ("\3\2\1\0\0\0\0\0"), == 3);
  T2 ("xxxxxxx", uint128_t, 0, 0, I64 ("\0\3\2\1\0\0\0\0"), == 0);

  uint64_t x4321     = I64 ("\4\3\2\1\0\0\0\0");
  uint64_t x54321    = I64 ("\5\4\3\2\1\0\0\0");
  uint64_t x654321   = I64 ("\6\5\4\3\2\1\0\0");
  uint64_t x7654321  = I64 ("\7\6\5\4\3\2\1\0");
  uint64_t x87654321 = I64 ("8\7\6\5\4\3\2\1");
  uint64_t x9        = I64 ("9\0\0\0\0\0\0\0");
  uint64_t xa9       = I64 ("a9\0\0\0\0\0\0");
  uint64_t xba9      = I64 ("ba9\0\0\0\0\0\0");
  uint64_t xcba9     = I64 ("cba9\0\0\0\0\0");
  uint64_t xdcba9    = I64 ("dcba9\0\0\0\0");
  uint64_t xedcba9   = I64 ("edcba9\0\0\0\0");
  uint64_t xfedcba9  = I64 ("fedcba9\0\0\0");

  T2 (0, uint128_t, 0,        0,     x4321, ==  4);
  T2 (0, uint128_t, 0,        0,    x54321, ==  5);
  T2 (0, uint128_t, 0,        0,   x654321, ==  6);
  T2 (0, uint128_t, 0,        0,  x7654321, ==  7);
  T2 (0, uint128_t, 0,        0, x87654321, ==  8);
  T2 (0, uint128_t, 0,       x9, x87654321, ==  9);
  T2 (0, uint128_t, 0,      xa9, x87654321, == 10);
  T2 (0, uint128_t, 0,     xba9, x87654321, == 11);
  T2 (0, uint128_t, 0,    xcba9, x87654321, == 12);
  T2 (0, uint128_t, 0,   xdcba9, x87654321, == 13);
  T2 (0, uint128_t, 0,  xedcba9, x87654321, == 14);
  T2 (0, uint128_t, 0, xfedcba9, x87654321, == 15);
}

#endif   // __uint128_t

/* { dg-final { scan-tree-dump-times "strlen" 0 "optimized" } }
   { dg-final { scan-tree-dump-times "_not_eliminated_" 0 "optimized" } } */
