/* PR tree-optimization/86711 - wrong folding of memchr

   Verify that memchr() of arrays initialized with string literals
   where the nul doesn't fit in the array doesn't find the nul.  */
typedef __SIZE_TYPE__  size_t;
typedef __WCHAR_TYPE__ wchar_t;

extern void* memchr (const void*, int, size_t);

#define A(expr)							\
  ((expr)							\
   ? (void)0							\
   : (__builtin_printf ("assertion failed on line %i: %s\n",	\
			__LINE__, #expr),			\
      __builtin_abort ()))

static const char c = '1';
static const char s1[1] = "1";
static const char s4[4] = "1234";

static const char s4_2[2][4] = { "1234", "5678" };
static const char s5_3[3][5] = { "12345", "6789", "01234" };

volatile int v0 = 0;
volatile int v1 = 1;
volatile int v2 = 2;
volatile int v3 = 3;
volatile int v4 = 3;

void test_narrow (void)
{
  int i0 = 0;
  int i1 = i0 + 1;
  int i2 = i1 + 1;
  int i3 = i2 + 1;
  int i4 = i3 + 1;

  A (memchr ("" + 1, 0, 0) == 0);

  A (memchr (&c, 0, sizeof c) == 0);
  A (memchr (&c + 1, 0, sizeof c - 1) == 0);
  A (memchr (&c + i1, 0, sizeof c - i1) == 0);
  A (memchr (&c + v1, 0, sizeof c - v1) == 0);

  A (memchr (s1, 0, sizeof s1) == 0);
  A (memchr (s1 + 1, 0, sizeof s1 - 1) == 0);
  A (memchr (s1 + i1, 0, sizeof s1 - i1) == 0);
  A (memchr (s1 + v1, 0, sizeof s1 - v1) == 0);

  A (memchr (&s1, 0, sizeof s1) == 0);
  A (memchr (&s1 + 1, 0, sizeof s1 - 1) == 0);
  A (memchr (&s1 + i1, 0, sizeof s1 - i1) == 0);
  A (memchr (&s1 + v1, 0, sizeof s1 - v1) == 0);

  A (memchr (&s1[0], 0, sizeof s1) == 0);
  A (memchr (&s1[0] + 1, 0, sizeof s1 - 1) == 0);
  A (memchr (&s1[0] + i1, 0, sizeof s1 - i1) == 0);
  A (memchr (&s1[0] + v1, 0, sizeof s1 - v1) == 0);

  A (memchr (&s1[i0], 0, sizeof s1) == 0);
  A (memchr (&s1[i0] + 1, 0, sizeof s1 - 1) == 0);
  A (memchr (&s1[i0] + i1, 0, sizeof s1 - i1) == 0);
  A (memchr (&s1[i0] + v1, 0, sizeof s1 - v1) == 0);

  A (memchr (&s1[v0], 0, sizeof s1) == 0);
  A (memchr (&s1[v0] + 1, 0, sizeof s1 - 1) == 0);
  A (memchr (&s1[v0] + i1, 0, sizeof s1 - i1) == 0);
  A (memchr (&s1[v0] + v1, 0, sizeof s1 - v1) == 0);


  A (memchr (s4 + i0, 0, sizeof s4 - i0) == 0);
  A (memchr (s4 + i1, 0, sizeof s4 - i1) == 0);
  A (memchr (s4 + i2, 0, sizeof s4 - i2) == 0);
  A (memchr (s4 + i3, 0, sizeof s4 - i3) == 0);
  A (memchr (s4 + i4, 0, sizeof s4 - i4) == 0);

  A (memchr (s4 + v0, 0, sizeof s4 - v0) == 0);
  A (memchr (s4 + v1, 0, sizeof s4 - v1) == 0);
  A (memchr (s4 + v2, 0, sizeof s4 - v2) == 0);
  A (memchr (s4 + v3, 0, sizeof s4 - v3) == 0);
  A (memchr (s4 + v4, 0, sizeof s4 - v4) == 0);


  A (memchr (s4_2, 0, sizeof s4_2) == 0);

  A (memchr (s4_2[0], 0, sizeof s4_2[0]) == 0);
  A (memchr (s4_2[1], 0, sizeof s4_2[1]) == 0);

  A (memchr (s4_2[0] + 1, 0, sizeof s4_2[0] - 1) == 0);
  A (memchr (s4_2[1] + 2, 0, sizeof s4_2[1] - 2) == 0);
  A (memchr (s4_2[1] + 3, 0, sizeof s4_2[1] - 3) == 0);

  A (memchr (s4_2[v0], 0, sizeof s4_2[v0]) == 0);
  A (memchr (s4_2[v0] + 1, 0, sizeof s4_2[v0] - 1) == 0);


  /* The following calls must find the nul.  */
  A (memchr ("", 0, 1) != 0);
  A (memchr (s5_3, 0, sizeof s5_3) == &s5_3[1][4]);

  A (memchr (&s5_3[0][0] + i0, 0, sizeof s5_3 - i0) == &s5_3[1][4]);
  A (memchr (&s5_3[0][0] + i1, 0, sizeof s5_3 - i1) == &s5_3[1][4]);
  A (memchr (&s5_3[0][0] + i2, 0, sizeof s5_3 - i2) == &s5_3[1][4]);
  A (memchr (&s5_3[0][0] + i4, 0, sizeof s5_3 - i4) == &s5_3[1][4]);

  A (memchr (&s5_3[1][i0], 0, sizeof s5_3[1] - i0) == &s5_3[1][4]);
}

#if 4 == __WCHAR_WIDTH__

static const wchar_t wc = L'1';
static const wchar_t ws1[] = L"1";
static const wchar_t ws4[] = L"\x00123456\x12005678\x12340078\x12345600";

void test_wide (void)
{
  int i0 = 0;
  int i1 = i0 + 1;
  int i2 = i1 + 1;
  int i3 = i2 + 1;
  int i4 = i3 + 1;

  A (memchr (L"" + 1, 0, 0) == 0);
  A (memchr (&wc + 1, 0, 0) == 0);
  A (memchr (L"\x12345678", 0, sizeof (wchar_t)) == 0);

  const size_t nb = sizeof ws4;
  const size_t nwb = sizeof (wchar_t);

  const char *pws1 = (const char*)ws1;
  const char *pws4 = (const char*)ws4;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  A (memchr (ws1, 0, sizeof ws1) == pws1 + 1);

  A (memchr (&ws4[0], 0, nb) == pws4 + 3);
  A (memchr (&ws4[1], 0, nb - 1 * nwb) == pws4 + 1 * nwb + 2);
  A (memchr (&ws4[2], 0, nb - 2 * nwb) == pws4 + 2 * nwb + 1);
  A (memchr (&ws4[3], 0, nb - 3 * nwb) == pws4 + 3 * nwb + 0);
#else
  A (memchr (ws1, 0, sizeof ws1) == pws1 + 0);

  A (memchr (&ws4[0], 0, nb) == pws4 + 0);
  A (memchr (&ws4[1], 0, nb - 1 * nwb) == pws4 + 1 * nwb + 1);
  A (memchr (&ws4[2], 0, nb - 2 * nwb) == pws4 + 2 * nwb + 2);
  A (memchr (&ws4[3], 0, nb - 3 * nwb) == pws4 + 3 * nwb + 3);
#endif
}

#elif 2 == __WCHAR_WIDTH__

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

#else

void test_wide (void) { }

#endif

int main ()
{
  test_narrow ();
  test_wide ();
}
