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

typedef __UINT16_TYPE__  uint16_t;
typedef __UINT32_TYPE__  uint32_t;
typedef __UINT64_TYPE__  uint64_t;
typedef __UINT64_TYPE__  uint64_t;

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

/* Same as T but works around the optimizer dropping the initializing
   store before the assignment and defeating the strlen optimization.  */
#define TX(init, type, off, assign, expect) do {		\
    char a[32];							\
    strcpy (a, init + 2);					\
    strcat (a, init + sizeof (init) - 3);			\
    *(type*)(a + off) = assign;					\
    ELIM (!(strlen (a) expect));				\
  } while (0)

/* Evaluates to an element at index I of the literal S padded with nuls
   on the right.  */
#define ELT(s, i)   ((s "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0")[i])

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
/* Form a big-endian 16, 32, 64, and 128-byte integer from a string.  */
#  define I16(s) (((uint16_t)ELT (s, 0) << 8) + (uint16_t)ELT (s, 1))
#  define I32(s)				\
  (((uint32_t)ELT (s, 0) << 24)			\
   + ((uint32_t)ELT (s, 1) << 16)		\
   + ((uint32_t)ELT (s, 2) << 8)		\
   + (uint32_t)ELT (s, 3))
#  define I64(s)				\
  (((uint64_t)ELT (s, 0) << 56)			\
   + ((uint64_t)ELT (s, 1) << 48)		\
   + ((uint64_t)ELT (s, 2) << 40)		\
   + ((uint64_t)ELT (s, 3) << 32)		\
   + ((uint64_t)ELT (s, 4) << 24)		\
   + ((uint64_t)ELT (s, 5) << 16)		\
   + ((uint64_t)ELT (s, 6) << 8)		\
   + ELT (s, 7))
#  define I128(s)				\
  (((uint128_t)ELT (s, 0) << (64 + 56))		\
   + ((uint128_t)ELT (s, 1) << (64 + 48))	\
   + ((uint128_t)ELT (s, 2) << (64 + 40))	\
   + ((uint128_t)ELT (s, 3) << (64 + 32))	\
   + ((uint128_t)ELT (s, 4) << (64 + 24))	\
   + ((uint128_t)ELT (s, 5) << (64 + 16))	\
   + ((uint128_t)ELT (s, 6) << (64 + 8))	\
   + ((uint128_t)ELT (s, 7) << 64)		\
   + ((uint128_t)ELT (s, 8) << 56)		\
   + ((uint128_t)ELT (s, 9) << 48)		\
   + ((uint128_t)ELT (s, 10) << 40)		\
   + ((uint128_t)ELT (s, 11) << 32)		\
   + ((uint128_t)ELT (s, 12) << 24)		\
   + ((uint128_t)ELT (s, 13) << 16)		\
   + ((uint128_t)ELT (s, 14) << 8)		\
   + (uint128_t)ELT (s, 15))

#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
/* Form a little-endian 16, 32, 64, and 128-byte integer from a string.  */
#  define I16(s) (((uint16_t)ELT (s, 1) << 8) + (uint16_t)ELT (s, 0))
#  define I32(s)				\
  (((uint32_t)ELT (s, 3) << 24)			\
   + ((uint32_t)ELT (s, 2) << 16)		\
   + ((uint32_t)ELT (s, 1) << 8)		\
   + (uint32_t)ELT (s, 0))
#  define I64(s)				\
  (((uint64_t)ELT (s, 7) << 56)			\
   + ((uint64_t)ELT (s, 6) << 48)		\
   + ((uint64_t)ELT (s, 5) << 40)		\
   + ((uint64_t)ELT (s, 4) << 32)		\
   + ((uint64_t)ELT (s, 3) << 24)		\
   + ((uint64_t)ELT (s, 2) << 16)		\
   + ((uint64_t)ELT (s, 1) << 8)		\
   + ELT (s, 0))
#  define I128(s)				\
  (((uint128_t)ELT (s, 15) << (64 + 56))	\
   + ((uint128_t)ELT (s, 14) << (64 + 48))	\
   + ((uint128_t)ELT (s, 13) << (64 + 40))	\
   + ((uint128_t)ELT (s, 12) << (64 + 32))	\
   + ((uint128_t)ELT (s, 11) << (64 + 24))	\
   + ((uint128_t)ELT (s, 10) << (64 + 16))	\
   + ((uint128_t)ELT (s, 9) << (64 + 8))	\
   + ((uint128_t)ELT (s, 8) << 64)		\
   + ((uint128_t)ELT (s, 7) << 56)		\
   + ((uint128_t)ELT (s, 6) << 48)		\
   + ((uint128_t)ELT (s, 5) << 40)		\
   + ((uint128_t)ELT (s, 4) << 32)		\
   + ((uint128_t)ELT (s, 3) << 24)		\
   + ((uint128_t)ELT (s, 2) << 16)		\
   + ((uint128_t)ELT (s, 1) << 8)		\
   + (uint128_t)ELT (s, 0))
#endif

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__

void store_16bit_be (void)
{
  T ("xxx", uint16_t, 0, 0x0001, == 0);
  T ("xxx", uint16_t, 0, 0x0010, == 0);
  T ("xxx", uint16_t, 0, 0x0011, == 0);
  T ("xxx", uint16_t, 0, 0x0100, == 1);
  T ("xxx", uint16_t, 0, 0x1000, == 1);
  T ("xxx", uint16_t, 0, 0x1100, == 1);
}

#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__

void store_16bit_le (int i)
{
  uint16_t x0000 = I16 ("\0\0");
  uint16_t x0001 = 0x0001;
  uint16_t x0010 = 0x0010;
  uint16_t x0011 = 0x0011;
  uint16_t x0100 = 0x0100;
  uint16_t x1000 = 0x1000;
  uint16_t x1100 = 0x1100;

  T (0,        uint16_t, 0, x0000, == 0);
  T ("x",      uint16_t, 0, x0000, == 0);
  T ("xx",     uint16_t, 0, x0000, == 0);
  T ("xxxx",   uint16_t, 0, x0000, == 0);
  T (0,        uint16_t, 0, x0001, == 1);
  T ("\0\0\0", uint16_t, 0, x0001, == 1);
  T (0,        uint16_t, 0, x0010, == 1);
  T ("x\0\0",  uint16_t, 0, x0010, == 1);
  T (0,        uint16_t, 0, x0011, == 1);
  T ("xx\0",   uint16_t, 0, x0011, == 1);
  T (0,        uint16_t, 0, x0100, == 0);
  T ("\0\0\0", uint16_t, 0, x0100, == 0);
  T (0,        uint16_t, 0, x1000, == 0);
  T ("x\0\0",  uint16_t, 0, x1000, == 0);
  T (0,        uint16_t, 0, x1100, == 0);
  T ("xx\0",   uint16_t, 0, x1100, == 0);

  // FIXME: This fails because of the next test but succeeds on its own.
  // T (0,        uint16_t, 0, i ? x0001 : x0010, == 1);
  T ("xxx",    uint16_t, 0, i ? x0100 : x1100, == 0);
}

#endif

void store_32bit (volatile int i)
{
  T (0,      uint32_t, 0, 0, == 0);
  T ("x",    uint32_t, 0, 0, == 0);
  T ("xx",   uint32_t, 0, 0, == 0);
  T ("xxx",  uint32_t, 0, 0, == 0);
  T ("xxxx", uint32_t, 0, 0, == 0);

  T ("\0",   uint32_t, 1, 0, == 0);
  T ("x",    uint32_t, 1, 0, == 1);
  T ("xx",   uint32_t, 2, 0, == 2);
  T ("xxx",  uint32_t, 3, 0, == 3);

  T ("xxx",  uint32_t, 0, I32 ("\1\0\0\0"), == 1);
  T ("xxx",  uint32_t, 0, I32 ("\0\1\0\0"), == 0);
  T ("xxx",  uint32_t, 0, I32 ("\0\0\1\0"), == 0);
  T ("xxx",  uint32_t, 0, I32 ("\0\0\0\1"), == 0);

  T ("xxx",  uint32_t, 0, I32 ("\1\2\0\0"), == 2);
  T ("xxx",  uint32_t, 0, I32 ("\0\1\2\0"), == 0);
  T ("xxx",  uint32_t, 0, I32 ("\0\0\1\2"), == 0);

  T ("xxx",  uint32_t, 0, I32 ("\1\2\3\0"), == 3);
  T ("xxx",  uint32_t, 0, I32 ("\0\1\2\3"), == 0);

  uint32_t x123_ = I32 ("123\0");
  uint32_t x12__ = I32 ("12\0\0");
  uint32_t x1___ = I32 ("1\0\0\0");

  // FIXME: Upper bound not implemented yet.
  /* T ("xxxx", uint32_t, 0, i ? x123_ : x12__, <= 3); */
  T ("xxxx", uint32_t, 0, i ? x123_ : x12__, >= 2);
  T ("xxxx", uint32_t, 0, i ? x12__ : x123_, >= 2);
  /* T ("xxxx", uint32_t, 0, i ? x123_ : x1___, <= 3); */
  T ("xxxx", uint32_t, 0, i ? x123_ : x1___, >= 1);
  T ("xxxx", uint32_t, 0, i ? x1___ : x123_, >= 1);

  TX ("abcde",  uint32_t, 0, i ? I32 ("1234") : I32 ("1235"), == 5);
  TX ("abcde",  uint32_t, 1, i ? I32 ("1234") : I32 ("1235"), == 5);

  TX ("abcdef", uint32_t, 0, i ? I32 ("1235") : I32 ("1234"), == 6);
  TX ("abcdef", uint32_t, 1, i ? I32 ("1235") : I32 ("1234"), == 6);
  TX ("abcdef", uint32_t, 2, i ? I32 ("1235") : I32 ("1234"), == 6);
  TX ("abcdef", uint32_t, 3, i ? I32 ("124\0") : I32 ("123\0"), == 6);
  TX ("abcdef", uint32_t, 3, i ? I32 ("12\0\0") : I32 ("13\0\0"), == 5);

  TX ("abcdef", uint32_t, 3, i ? I32 ("12\0\0") : I32 ("123\0"), >= 5);
  /* FIXME: Upper bound not implemented yet.  */
  /* TX ("abcdef", uint32_t, 3, i ? I32 ("12\0\0") : I32 ("123\0"), < 7); */
}

void store_64bit (int i)
{
  T ("xxxxxxx", uint64_t, 0, I64 ("\1\0\0\0\0\0\0\0\0"), == 1);
  T ("xxxxxxx", uint64_t, 0, I64 ("\0\1\0\0\0\0\0\0\0"), == 0);
  T ("xxxxxxx", uint64_t, 0, I64 ("\0\0\1\0\0\0\0\0\0"), == 0);
  T ("xxxxxxx", uint64_t, 0, I64 ("\0\0\0\1\0\0\0\0\0"), == 0);
  T ("xxxxxxx", uint64_t, 0, I64 ("\0\0\0\0\1\0\0\0\0"), == 0);
  T ("xxxxxxx", uint64_t, 0, I64 ("\0\0\0\0\0\1\0\0\0"), == 0);
  T ("xxxxxxx", uint64_t, 0, I64 ("\0\0\0\0\0\0\1\0\0"), == 0);
  T ("xxxxxxx", uint64_t, 0, I64 ("\0\0\0\0\0\0\0\1\0"), == 0);

  T ("xxxxxxx", uint64_t, 0, I64 ("\1\2\0\0\0\0\0\0\0"), == 2);
  T ("xxxxxxx", uint64_t, 0, I64 ("\0\1\2\0\0\0\0\0\0"), == 0);
  T ("xxxxxxx", uint64_t, 0, I64 ("\0\0\1\2\0\0\0\0\0"), == 0);

  T ("xxxxxxx", uint64_t, 0, I64 ("\1\2\3\0\0\0\0\0\0"), == 3);
  T ("xxxxxxx", uint64_t, 0, I64 ("\0\1\2\3\0\0\0\0\0"), == 0);

  T ("xxxxxxx", uint64_t, 0, I64 ("\1\2\3\4\0\0\0\0\0"), == 4);
  T ("xxxxxxx", uint64_t, 0, I64 ("\1\2\3\4\5\0\0\0\0"), == 5);
  T ("xxxxxxx", uint64_t, 0, I64 ("\1\2\3\4\5\6\0\0\0"), == 6);
  T ("xxxxxxx", uint64_t, 0, I64 ("\1\2\3\4\5\6\7\0\0"), == 7);

  uint64_t x7777777_ = I64 ("\7\7\7\7\7\7\7");
  uint64_t x666666__ = I64 ("\6\6\6\6\6\6\0");
  uint64_t x4444____ = I64 ("\4\4\4\4\0\0\0");
  uint64_t x4343____ = I64 ("\4\3\4\3\0\0\0");
  uint64_t x1_______ = I64 ("\1\0\0\0\0\0\0");

  /* FIXME: Upper bound not implemented yet.  */
  /* T ("x\0xxxxxx", uint64_t, 0, i ? x7777777_ : x666666__, <= 7); */
  T ("xx\0xxxxx", uint64_t, 0, i ? x7777777_ : x666666__, >= 6);
  T ("xxx\0xxxx", uint64_t, 1, i ? x7777777_ : x666666__, >= 7);
  /* T ("xxx\0xxxx", uint64_t, 0, i ? x666666__ : x1, <= 6); */
  T ("xxxx\0xxx", uint64_t, 0, i ? x666666__ : x1_______, >= 1);
  T ("xxxxxx\0x", uint64_t, 0, i ? x4444____ : x4343____, == 4);
}

#if __SIZEOF_INT128__

typedef __uint128_t     uint128_t;

void store_128bit (void)
{
  uint128_t x1    = I128 ("\1");
  uint128_t x1z1  = I128 ("\0\1");
  uint128_t x2z1  = I128 ("\0\0\1");
  uint128_t x3z1  = I128 ("\0\0\0\1");
  uint128_t x4z1  = I128 ("\0\0\0\0\1");
  uint128_t x5z1  = I128 ("\0\0\0\0\0\1");
  uint128_t x6z1  = I128 ("\0\0\0\0\0\0\1");
  uint128_t x7z1  = I128 ("\0\0\0\0\0\0\0\1");
  uint128_t x8z1  = I128 ("\0\0\0\0\0\0\0\0\1");
  uint128_t x9z1  = I128 ("\0\0\0\0\0\0\0\0\0\1");
  uint128_t x10z1 = I128 ("\0\0\0\0\0\0\0\0\0\0\1");
  uint128_t x11z1 = I128 ("\0\0\0\0\0\0\0\0\0\0\0\1");
  uint128_t x12z1 = I128 ("\0\0\0\0\0\0\0\0\0\0\0\0\1");
  uint128_t x13z1 = I128 ("\0\0\0\0\0\0\0\0\0\0\0\0\0\1");
  uint128_t x14z1 = I128 ("\0\0\0\0\0\0\0\0\0\0\0\0\0\0\1");
  uint128_t x15z1 = I128 ("\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\1");

  T ("xxxxxxx", uint128_t, 0, x1, == 1);
  T ("xxxxxxx", uint128_t, 0, x1z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x2z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x3z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x4z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x5z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x6z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x7z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x8z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x9z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x10z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x11z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x12z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x13z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x14z1, == 0);
  T ("xxxxxxx", uint128_t, 0, x15z1, == 0);

  T ("xxxxxxx", uint128_t, 0, I128 ("\2\1"), == 2);
  T ("xxxxxxx", uint128_t, 0, I128 ("\0\2\1"), == 0);

  T ("xxxxxxx", uint128_t, 0, I128 ("\3\2\1"), == 3);
  T ("xxxxxxx", uint128_t, 0, I128 ("\0\3\2\1"), == 0);

  T ("xxxxxxx", uint128_t, 0, I128 ("\1\2\3\4"), == 4);
  T ("xxxxxxx", uint128_t, 0, I128 ("\1\2\3\4\5"), == 5);
  T ("xxxxxxx", uint128_t, 0, I128 ("\1\2\3\4\5\6"), == 6);
  T ("xxxxxxx", uint128_t, 0, I128 ("\1\2\3\4\5\6\7"), == 7);
  T ("xxxxxxx", uint128_t, 0, I128 ("\1\2\3\4\5\6\7\10"), == 8);
  T ("xxxxxxx", uint128_t, 0, I128 ("\1\2\3\4\5\6\7\10\11"), == 9);
  T ("xxxxxxx", uint128_t, 0, I128 ("\1\2\3\4\5\6\7\10\11\12"), == 10);
  T ("xxxxxxx", uint128_t, 0, I128 ("\1\2\3\4\5\6\7\10\11\12\13"), == 11);
  T ("xxxxxxx", uint128_t, 0, I128 ("\1\2\3\4\5\6\7\10\11\12\13\14"), == 12);
  T ("xxxxxxx", uint128_t, 0, I128 ("\1\2\3\4\5\6\7\10\11\12\13\14\15"), == 13);
  T ("xxxxxxx", uint128_t, 0, I128 ("\1\2\3\4\5\6\7\10\11\12\13\14\15\16"), == 14);
  T ("xxxxxxx", uint128_t, 0, I128 ("\1\2\3\4\5\6\7\10\11\12\13\14\15\16\17"), == 15);
}

#endif   // __SIZEOF_INT128__

/* { dg-final { scan-tree-dump-times "strlen" 0 "optimized" } }
   { dg-final { scan-tree-dump-times "_not_eliminated_" 0 "optimized" } } */
