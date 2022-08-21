/* P2290R3 - Delimited escape sequences */
/* { dg-do run } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=gnu99 -Wno-c++-compat" { target c } } */
/* { dg-options "-std=c++23" { target c++ } } */

#ifndef __cplusplus
#include <wchar.h>
typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;
#endif

const char32_t *a = U"\u{1234}\u{10fffd}\u{000000000000000000000000000000000000000000000000000000000001234}\u{10FFFD}";
const char32_t *b = U"\x{1234}\x{10fffd}\x{000000000000000000000000000000000000000000000000000000000001234}";
const char32_t *c = U"\o{1234}\o{4177775}\o{000000000000000000000000000000000000000000000000000000000000000000000000004177775}";
const char16_t *d = u"\u{1234}\u{bFFd}\u{00000000000000000000000000000001234}";
const char16_t *e = u"\x{1234}\x{BffD}\x{000001234}";
const char16_t *f = u"\o{1234}\o{137775}\o{000000000000000137775}";
const wchar_t *g = L"\u{1234}\u{bFFd}\u{00000000000000000000000000000001234}";
const wchar_t *h = L"\x{1234}\x{bFFd}\x{000001234}";
const wchar_t *i = L"\o{1234}\o{137775}\o{000000000000000137775}";
#ifdef __cplusplus
const char *j = "\u{34}\u{000000000000000003D}";
#endif
const char *k = "\x{34}\x{000000000000000003D}";
const char *l = "\o{34}\o{000000000000000176}";

#if U'\u{1234}' != U'\u1234' || U'\u{10fffd}' != U'\U0010FFFD' \
    || U'\x{00000001234}' != U'\x1234' || U'\x{010fffd}' != U'\x10FFFD' \
    || U'\o{1234}' != U'\x29c' || U'\o{004177775}' != U'\x10FFFD' \
    || u'\u{1234}' != u'\u1234' || u'\u{0bffd}' != u'\uBFFD' \
    || u'\x{00000001234}' != u'\x1234' || u'\x{0Bffd}' != u'\x0bFFD' \
    || u'\o{1234}' != u'\x29c' || u'\o{00137775}' != u'\xBFFD' \
    || L'\u{1234}' != L'\u1234' || L'\u{0bffd}' != L'\uBFFD' \
    || L'\x{00000001234}' != L'\x1234' || L'\x{0bffd}' != L'\x0bFFD' \
    || L'\o{1234}' != L'\x29c' || L'\o{00137775}' != L'\xBFFD' \
    || '\x{34}' != '\x034' || '\x{0003d}' != '\x003D' \
    || '\o{34}' != '\x1C' || '\o{176}' != '\x007E'
#error Bad
#endif
#ifdef __cplusplus
#if '\u{0000000034}' != '\u0034' || '\u{3d}' != '\u003D'
#error Bad
#endif
#endif

int
main ()
{
  if (a[0] != U'\u1234' || a[0] != U'\u{1234}'
      || a[1] != U'\U0010FFFD' || a[1] != U'\u{000010fFfD}'
      || a[2] != a[0]
      || a[3] != a[1]
      || b[0] != U'\x1234' || b[0] != U'\x{001234}'
      || b[1] != U'\x10FFFD' || b[1] != U'\x{0010fFfD}'
      || b[2] != b[0]
      || c[0] != U'\x29c' || c[0] != U'\o{001234}'
      || c[1] != U'\x10FFFD' || c[1] != U'\o{4177775}'
      || c[2] != c[1])
    __builtin_abort ();
  if (d[0] != u'\u1234' || d[0] != u'\u{1234}'
      || d[1] != u'\U0000BFFD' || d[1] != u'\u{00000bFfD}'
      || d[2] != d[0]
      || e[0] != u'\x1234' || e[0] != u'\x{001234}'
      || e[1] != u'\xBFFD' || e[1] != u'\x{00bFfD}'
      || e[2] != e[0]
      || f[0] != u'\x29c' || f[0] != u'\o{001234}'
      || f[1] != u'\xbFFD' || f[1] != u'\o{137775}'
      || f[2] != f[1])
    __builtin_abort ();
  if (g[0] != L'\u1234' || g[0] != L'\u{1234}'
      || g[1] != L'\U0000BFFD' || g[1] != L'\u{00000bFfD}'
      || g[2] != g[0]
      || h[0] != L'\x1234' || h[0] != L'\x{001234}'
      || h[1] != L'\xBFFD' || h[1] != L'\x{00bFfD}'
      || h[2] != h[0]
      || i[0] != L'\x29c' || i[0] != L'\o{001234}'
      || i[1] != L'\xbFFD' || i[1] != L'\o{137775}'
      || i[2] != i[1])
    __builtin_abort ();
#ifdef __cplusplus
  if (j[0] != '\u0034' || j[0] != '\u{034}'
      || j[1] != '\U0000003D' || j[1] != '\u{000003d}')
    __builtin_abort ();
#endif
  if (k[0] != '\x034' || k[0] != '\x{0034}'
      || k[1] != '\x3D' || k[1] != '\x{3d}'
      || l[0] != '\x1c' || l[0] != '\o{0034}'
      || l[1] != '\x07e' || l[1] != '\o{176}' || l[1] != '\176')
    __builtin_abort ();
  return 0;
}
