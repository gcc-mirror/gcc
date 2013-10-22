/* Test Unicode strings in C11.  Test valid code.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

/* More thorough tests are in c-c++-common/raw-string-*.c; this test
   verifies the particular subset (Unicode but not raw strings) that
   is in C11.  */

typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;
typedef __SIZE_TYPE__ size_t;

extern void abort (void);
extern void exit (int);
extern int memcmp (const void *, const void *, size_t);

#define R "(R)"
#define u8R "(u8R)"
#define uR "(uR)"
#define UR "(UR)"
#define LR "(LR)"
#define u8 randomu8
#define u randomu
#define U randomU

const char su8[] = u8"a\u010d";
const char su8a[] = "a\xc4\x8d";

const char16_t su16[] = u"\u0567";
const char16_t su16a[] = { 0x0567, 0 };

const char32_t su32[] = U"\u0123";
const char32_t su32a[] = { 0x0123, 0 };

const char tu[] = R"a";
const char tua[] = "(R)a";

const char tu8[] = u8R"b";
const char tu8a[] = "(u8R)b";

const char tu16[] = uR"c";
const char tu16a[] = "(uR)c";

const char tu32[] = UR"d";
const char tu32a[] = "(UR)d";

const char tl[] = LR"e";
const char tla[] = "(LR)e";

#define str(x) #x
const char ts[] = str(u"a" U"b" u8"c");
const char tsa[] = "u\"a\" U\"b\" u8\"c\"";

/* GCC always uses UTF-16 and UTF-32 for char16_t and char32_t.  */
#ifndef __STDC_UTF_16__
#error "__STDC_UTF_16__ not defined"
#endif
#ifndef __STDC_UTF_32__
#error "__STDC_UTF_32__ not defined"
#endif
#define xstr(x) str(x)
const char tm16[] = xstr(__STDC_UTF_16__);
const char tm16a[] = "1";
const char tm32[] = xstr(__STDC_UTF_32__);
const char tm32a[] = "1";

int
main (void)
{
  if (sizeof (su8) != sizeof (su8a)
      || memcmp (su8, su8a, sizeof (su8)) != 0)
    abort ();
  if (sizeof (su16) != sizeof (su16a)
      || memcmp (su16, su16a, sizeof (su16)) != 0)
    abort ();
  if (sizeof (su32) != sizeof (su32a)
      || memcmp (su32, su32a, sizeof (su32)) != 0)
    abort ();
  if (sizeof (tu) != sizeof (tua)
      || memcmp (tu, tua, sizeof (tu)) != 0)
    abort ();
  if (sizeof (tu8) != sizeof (tu8a)
      || memcmp (tu8, tu8a, sizeof (tu8)) != 0)
    abort ();
  if (sizeof (tu16) != sizeof (tu16a)
      || memcmp (tu16, tu16a, sizeof (tu16)) != 0)
    abort ();
  if (sizeof (tu32) != sizeof (tu32a)
      || memcmp (tu32, tu32a, sizeof (tu32)) != 0)
    abort ();
  if (sizeof (tl) != sizeof (tla)
      || memcmp (tl, tla, sizeof (tl)) != 0)
    abort ();
  if (sizeof (ts) != sizeof (tsa)
      || memcmp (ts, tsa, sizeof (ts)) != 0)
    abort ();
  if (sizeof (tm16) != sizeof (tm16a)
      || memcmp (tm16, tm16a, sizeof (tm16)) != 0)
    abort ();
  if (sizeof (tm32) != sizeof (tm32a)
      || memcmp (tm32, tm32a, sizeof (tm32)) != 0)
    abort ();
  if (u'\u0123' != 0x0123)
    abort ();
  if (U'\u0456' != 0x0456)
    abort ();
#undef u8
#define u8
  if (u8'a' != 'a')
    abort ();
  exit (0);
}
