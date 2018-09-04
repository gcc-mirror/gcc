/* PR tree-optimization/86711 - wrong folding of memchr

   Verify that calls to memchr() with constant arrays initialized
   with wide string literals are folded.

   { dg-do compile }
   { dg-options "-O1 -Wall -fdump-tree-optimized" } */

#include "strlenopt.h"

typedef __WCHAR_TYPE__ wchar_t;

extern void* memchr (const void*, int, size_t);

#define CONCAT(x, y) x ## y
#define CAT(x, y) CONCAT (x, y)
#define FAILNAME(name) CAT (call_ ## name ##_on_line_, __LINE__)

#define FAIL(name) do {				\
    extern void FAILNAME (name) (void);		\
    FAILNAME (name)();				\
  } while (0)

/* Macro to emit a call to funcation named
   call_in_true_branch_not_eliminated_on_line_NNN()
   for each call that's expected to be eliminated.  The dg-final
   scan-tree-dump-time directive at the bottom of the test verifies
   that no such call appears in output.  */
#define ELIM(expr)							\
  if (!(expr)) FAIL (in_true_branch_not_eliminated); else (void)0

#define T(s, n) ELIM (strlen (s) == n)


static const wchar_t wc = L'1';
static const wchar_t ws1[] = L"1";
static const wchar_t wsx[] = L"\x12345678";
static const wchar_t ws4[] = L"\x00123456\x12005678\x12340078\x12345600";

void test_wide (void)
{
  int i0 = 0;
  int i1 = i0 + 1;
  int i2 = i1 + 1;
  int i3 = i2 + 1;
  int i4 = i3 + 1;

  ELIM (memchr (L"" + 1, 0, 0) == 0);
  ELIM (memchr (&wc + 1, 0, 0) == 0);
  ELIM (memchr (L"\x12345678", 0, sizeof (wchar_t)) == 0);

  const size_t nb = sizeof ws4;
  const size_t nwb = sizeof (wchar_t);

  const char *pws1 = (const char*)ws1;
  const char *pws4 = (const char*)ws4;
  const char *pwsx = (const char*)wsx;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  ELIM (memchr (ws1, 0, sizeof ws1) == pws1 + 1);
  ELIM (memchr (wsx, 0, sizeof wsx) == pwsx + sizeof *wsx);

  ELIM (memchr (&ws4[0], 0, nb) == pws4 + 3);
  ELIM (memchr (&ws4[1], 0, nb - 1 * nwb) == pws4 + 1 * nwb + 2);
  ELIM (memchr (&ws4[2], 0, nb - 2 * nwb) == pws4 + 2 * nwb + 1);
  ELIM (memchr (&ws4[3], 0, nb - 3 * nwb) == pws4 + 3 * nwb + 0);
  ELIM (memchr (&ws4[4], 0, nb - 4 * nwb) == pws4 + 4 * nwb + 0);

  ELIM (memchr (&ws4[i0], 0, nb) == pws4 + 3);
  ELIM (memchr (&ws4[i1], 0, nb - 1 * nwb) == pws4 + 1 * nwb + 2);
  ELIM (memchr (&ws4[i2], 0, nb - 2 * nwb) == pws4 + 2 * nwb + 1);
  ELIM (memchr (&ws4[i3], 0, nb - 3 * nwb) == pws4 + 3 * nwb + 0);
  ELIM (memchr (&ws4[i4], 0, nb - 4 * nwb) == pws4 + 4 * nwb + 0);
#else
  ELIM (memchr (ws1, 0, sizeof ws1) == pws1 + 0);
  ELIM (memchr (wsx, 0, sizeof wsx) == pwsx + sizeof *wsx);

  ELIM (memchr (&ws4[0], 0, nb) == pws4 + 0);
  ELIM (memchr (&ws4[1], 0, nb - 1 * nwb) == pws4 + 1 * nwb + 1);
  ELIM (memchr (&ws4[2], 0, nb - 2 * nwb) == pws4 + 2 * nwb + 2);
  ELIM (memchr (&ws4[3], 0, nb - 3 * nwb) == pws4 + 3 * nwb + 3);
  ELIM (memchr (&ws4[4], 0, nb - 4 * nwb) == pws4 + 4 * nwb + 0);

  ELIM (memchr (&ws4[i0], 0, nb) == pws4 + 0);
  ELIM (memchr (&ws4[i1], 0, nb - 1 * nwb) == pws4 + 1 * nwb + 1);
  ELIM (memchr (&ws4[i2], 0, nb - 2 * nwb) == pws4 + 2 * nwb + 2);
  ELIM (memchr (&ws4[i3], 0, nb - 3 * nwb) == pws4 + 3 * nwb + 3);
  ELIM (memchr (&ws4[i4], 0, nb - 4 * nwb) == pws4 + 4 * nwb + 0);
#endif
}

/* { dg-final { scan-tree-dump-times "memchr" 0 "optimized" } }
   { dg-final { scan-tree-dump-times "call_in_true_branch_not_eliminated" 0 "optimized" } } */
