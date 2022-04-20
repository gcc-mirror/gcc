/* PR tree-optimization/91183 - strlen of a strcpy result with a conditional
   source not folded
   Test to verify that strlen can determine string lengths from stores
   involving PHI nodes with distinct strings of the same length of at
   least 16 bytes.

   { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized" }
   On strictly aligned targets the consecutive char assignments used
   by the test aren't merged.  When they involve multiple trailing nuls
   these assignments then defeat the strlen optimization as a result of
   pr83821.  When the bug is resolved the directive below can be removed.
   { dg-require-effective-target non_strict_align } */

#include "strlenopt.h"

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
#define ELIM(expr)				\
  if ((expr)) FAIL (not_eliminated); else (void)0

#define T(expect, N, ncpy, cond) do {		\
    char CONCAT (arr_, __LINE__)[N];		\
    char *pa = CONCAT (arr_, __LINE__);		\
    memcpy (pa, cond, ncpy);			\
    ELIM (!(expect strlen (pa)));		\
    sink (pa);					\
  } while (0)

void sink (void*);

const char a32[33] = "0123456789abcdef0123456789abcdef";
const char b32[33] = "fedcba9876543210fedcba9876543210";

const char a16[33] = "0123456789abcdef";
const char b16[33] = "fedcba9876543210";

int i0, i1, i2;

void test_copy_cond_equal_length (void)
{
  // The test below is represented as this:
  //   # iftmp.0_3 = PHI <&b16(2), &a16(3)>
  //   MEM <unsigned char[17]> [(char * {ref-all})&a]
  //     = MEM <unsigned char[17]> [(char * {ref-all})iftmp.0_3];
  //   _2 = strlen (&a);
  T (16 ==, 17, 17, i0 ? a16 : b16);
  T (16 ==, 17, 17, i0 ? a16 : b16);
  T (15 ==, 17, 16, (i0 ? a16 : b16) +  1);
  T (14 ==, 17, 15, (i0 ? a16 : b16) +  2);
  T ( 0 ==, 17,  1, (i0 ? a16 : b16) + 16);

  T (31 ==, 33, 32, (i0 ? a32 : b32) +  1);
  T (30 ==, 33, 31, (i0 ? a32 : b32) +  2);
  T (29 ==, 33, 30, (i0 ? a32 : b32) +  3);
  T ( 1 ==, 33,  2, (i0 ? a32 : b32) + 31);
  T ( 0 ==, 33,  1, (i0 ? a32 : b32) + 32);
}

#if (defined(__i386__) && defined(__SSE__)) || defined(__x86_64__) || defined(__aarch64__) \
    || defined(__s390__) || defined(__powerpc64__)

/* The following tests assume GCC transforms the memcpy calls into
   long long assignments which it does only on targets that define
   the MOVE_MAX macro to 8 or higher.  Enable on a set of targets
   known to do that.  */

const char a4[16] = "0123";
const char b4[16] = "3210";

void test_copy_cond_unequal_length_i64 (void)
{
  T (2 <, 16, 8, i0 ? a4 + 1 : b4 + 0);
  T (1 <, 16, 8, i0 ? a4 + 1 : b4 + 2);
  T (0 <, 16, 8, i0 ? a4 + 1 : b4 + 3);

  T (1 <, 16, 8, i0 ? a4 + 2 : b4 + 0);
  T (1 <, 16, 8, i0 ? a4 + 2 : b4 + 1);
  T (0 <, 16, 8, i0 ? a4 + 2 : b4 + 3);
}

#endif


#if defined(__x86_64__) && __SIZEOF_INT128__ == 16

/* The following tests assume GCC transforms the memcpy calls into
   int128_t assignments which it does only on targets that define
   the MOVE_MAX macro to 16.  That's only s390 and x86_64 with
   int128_t support.  */

const char a8[32] = "01234567";
const char b8[32] = "76543210";

void test_copy_cond_unequal_length_i128 (void)
{
  T (6 <, 32, 16, i0 ? a8 + 1 : b8 + 0);
  T (5 <, 32, 16, i0 ? a8 + 1 : b8 + 2);
  T (4 <, 32, 16, i0 ? a8 + 1 : b8 + 3);
  T (3 <, 32, 16, i0 ? a8 + 1 : b8 + 4);
  T (2 <, 32, 16, i0 ? a8 + 1 : b8 + 5);
  T (1 <, 32, 16, i0 ? a8 + 1 : b8 + 6);
  T (0 <, 32, 16, i0 ? a8 + 1 : b8 + 7);

  T (5 <, 32, 16, i0 ? a8 + 2 : b8 + 0);
  T (5 <, 32, 16, i0 ? a8 + 2 : b8 + 1);
  T (3 <, 32, 16, i0 ? a8 + 2 : b8 + 3);
  T (2 <, 32, 16, i0 ? a8 + 2 : b8 + 4);
  T (1 <, 32, 16, i0 ? a8 + 2 : b8 + 5);
  T (0 <, 32, 16, i0 ? a8 + 2 : b8 + 6);

  T (4 <, 32, 16, i0 ? a8 + 3 : b8 + 0);
  T (4 <, 32, 16, i0 ? a8 + 3 : b8 + 1);
  T (4 <, 32, 16, i0 ? a8 + 3 : b8 + 2);
  T (3 <, 32, 16, i0 ? a8 + 3 : b8 + 4);
  T (2 <, 32, 16, i0 ? a8 + 3 : b8 + 5);
  T (1 <, 32, 16, i0 ? a8 + 3 : b8 + 6);
  T (0 <, 32, 16, i0 ? a8 + 3 : b8 + 7);

  T (3 <, 32, 16, i0 ? a8 + 4 : b8 + 0);
  T (3 <, 32, 16, i0 ? a8 + 4 : b8 + 1);
  T (3 <, 32, 16, i0 ? a8 + 4 : b8 + 2);
  T (3 <, 32, 16, i0 ? a8 + 4 : b8 + 3);
  T (2 <, 32, 16, i0 ? a8 + 4 : b8 + 5);
  T (1 <, 32, 16, i0 ? a8 + 4 : b8 + 6);
  T (0 <, 32, 16, i0 ? a8 + 4 : b8 + 7);
}

#endif   /* Support for i128_t stores.  */

/* { dg-final { scan-tree-dump-times "strlen" 0 "optimized" } }
   { dg-final { scan-tree-dump-times "_not_eliminated_" 0 "optimized" } } */
