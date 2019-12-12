/* PR tree-optimization/86711 - wrong folding of memchr

   Verify that memchr() of arrays initialized with 16-bit wide string
   literals finds the nul only when it is present in the wide string.

   { dg-do compile }
   { dg-options "-O1 -Wall -fshort-wchar -fdump-tree-optimized" } */

typedef __SIZE_TYPE__  size_t;
typedef __WCHAR_TYPE__ wchar_t;

extern void* memchr (const void*, int, size_t);
extern int printf (const char*, ...);
extern void abort (void);

#define A(expr)							\
  ((expr)							\
   ? (void)0							\
   : (printf ("assertion failed on line %i: %s\n",		\
			__LINE__, #expr),			\
      abort ()))

static const wchar_t wc = L'1';
static const wchar_t ws1[] = L"1";
static const wchar_t ws2[2] = L"\x1234\x5678";   /* no terminating nul */
static const wchar_t ws4[] = L"\x0012\x1200\x1234";

void test_wide (void)
{
  int i0 = 0;
  int i1 = i0 + 1;
  int i2 = i1 + 1;

  A (sizeof (wchar_t) == 2);

  A (memchr (L"" + 1, 0, 0) == 0);
  A (memchr (&wc + 1, 0, 0) == 0);
  A (memchr (L"\x1234", 0, sizeof (wchar_t)) == 0);

  A (memchr (L"" + i1, i0, i0) == 0);
  A (memchr (&wc + i1, i0, i0) == 0);
  A (memchr (L"\x1234", i0, sizeof (wchar_t)) == 0);

  A (memchr (ws2, 0, sizeof ws2) == 0);
  A (memchr (ws2, i0, sizeof ws2) == 0);

  const size_t nb = sizeof ws4;
  const size_t nwb = sizeof (wchar_t);

  const char *pws1 = (const char*)ws1;
  const char *pws4 = (const char*)ws4;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  A (memchr (ws1, i0, sizeof ws1) == pws1 + 1);

  A (memchr (&ws4[0], i0, nb) == pws4 + i1);
  A (memchr (&ws4[1], i0, nb - i1 * nwb) == pws4 + i1 * nwb);
  A (memchr (&ws4[2], i0, nb - i2 * nwb) == pws4 + i2 * nwb + i2);
#else
  A (memchr (ws1, i0, sizeof ws1) == pws1 + 0);

  A (memchr (&ws4[0], i0, nb) == pws4 + 0);
  A (memchr (&ws4[1], i0, nb - i1 * nwb) == pws4 + i1 * nwb + i1);
  A (memchr (&ws4[2], i0, nb - i2 * nwb) == pws4 + i2 * nwb + i2);
#endif
}

/* { dg-final { scan-tree-dump-not "abort" "optimized" } } */
