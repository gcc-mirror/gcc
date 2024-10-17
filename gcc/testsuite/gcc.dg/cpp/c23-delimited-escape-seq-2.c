/* N3353 - Delimited escape sequences */
/* { dg-do compile } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=c23 -pedantic-errors -Wno-c++-compat" } */

#include <wchar.h>
typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;

const char32_t *a = U"\u{1234}\u{10fffd}\u{000000000000000000000000000000000000000000000000000000000001234}\u{10FFFD}";		/* { dg-error "delimited escape sequences are only valid in" } */
const char32_t *b = U"\x{1234}\x{10fffd}\x{000000000000000000000000000000000000000000000000000000000001234}";			/* { dg-error "delimited escape sequences are only valid in" } */
const char32_t *c = U"\o{1234}\o{4177775}\o{000000000000000000000000000000000000000000000000000000000000000000000000004177775}";/* { dg-error "delimited escape sequences are only valid in" } */
const char16_t *d = u"\u{1234}\u{bFFd}\u{00000000000000000000000000000001234}";							/* { dg-error "delimited escape sequences are only valid in" } */
const char16_t *e = u"\x{1234}\x{BffD}\x{000001234}";										/* { dg-error "delimited escape sequences are only valid in" } */
const char16_t *f = u"\o{1234}\o{137775}\o{000000000000000137775}";								/* { dg-error "delimited escape sequences are only valid in" } */
const wchar_t *g = L"\u{1234}\u{bFFd}\u{00000000000000000000000000000001234}";							/* { dg-error "delimited escape sequences are only valid in" } */
const wchar_t *h = L"\x{1234}\x{bFFd}\x{000001234}";										/* { dg-error "delimited escape sequences are only valid in" } */
const wchar_t *i = L"\o{1234}\o{137775}\o{000000000000000137775}";								/* { dg-error "delimited escape sequences are only valid in" } */
const char *k = "\x{34}\x{000000000000000003D}";										/* { dg-error "delimited escape sequences are only valid in" } */
const char *l = "\o{34}\o{000000000000000176}";											/* { dg-error "delimited escape sequences are only valid in" } */

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
/* { dg-error "delimited escape sequences are only valid in" "" { target *-*-* } .-11 } */
/* { dg-error "delimited escape sequences are only valid in" "" { target *-*-* } .-11 } */
/* { dg-error "delimited escape sequences are only valid in" "" { target *-*-* } .-11 } */
/* { dg-error "delimited escape sequences are only valid in" "" { target *-*-* } .-11 } */
/* { dg-error "delimited escape sequences are only valid in" "" { target *-*-* } .-11 } */
/* { dg-error "delimited escape sequences are only valid in" "" { target *-*-* } .-11 } */
/* { dg-error "delimited escape sequences are only valid in" "" { target *-*-* } .-11 } */
/* { dg-error "delimited escape sequences are only valid in" "" { target *-*-* } .-11 } */
/* { dg-error "delimited escape sequences are only valid in" "" { target *-*-* } .-11 } */
/* { dg-error "delimited escape sequences are only valid in" "" { target *-*-* } .-11 } */
/* { dg-error "delimited escape sequences are only valid in" "" { target *-*-* } .-11 } */
#error Bad
#endif

int
main ()
{
  if (a[0] != U'\u1234' || a[0] != U'\u{1234}'					/* { dg-error "delimited escape sequences are only valid in" } */
      || a[1] != U'\U0010FFFD' || a[1] != U'\u{000010fFfD}'			/* { dg-error "delimited escape sequences are only valid in" } */
      || a[2] != a[0]
      || a[3] != a[1]
      || b[0] != U'\x1234' || b[0] != U'\x{001234}'				/* { dg-error "delimited escape sequences are only valid in" } */
      || b[1] != U'\x10FFFD' || b[1] != U'\x{0010fFfD}'				/* { dg-error "delimited escape sequences are only valid in" } */
      || b[2] != b[0]
      || c[0] != U'\x29c' || c[0] != U'\o{001234}'				/* { dg-error "delimited escape sequences are only valid in" } */
      || c[1] != U'\x10FFFD' || c[1] != U'\o{4177775}'				/* { dg-error "delimited escape sequences are only valid in" } */
      || c[2] != c[1])
    __builtin_abort ();
  if (d[0] != u'\u1234' || d[0] != u'\u{1234}'					/* { dg-error "delimited escape sequences are only valid in" } */
      || d[1] != u'\U0000BFFD' || d[1] != u'\u{00000bFfD}'			/* { dg-error "delimited escape sequences are only valid in" } */
      || d[2] != d[0]
      || e[0] != u'\x1234' || e[0] != u'\x{001234}'				/* { dg-error "delimited escape sequences are only valid in" } */
      || e[1] != u'\xBFFD' || e[1] != u'\x{00bFfD}'				/* { dg-error "delimited escape sequences are only valid in" } */
      || e[2] != e[0]
      || f[0] != u'\x29c' || f[0] != u'\o{001234}'				/* { dg-error "delimited escape sequences are only valid in" } */
      || f[1] != u'\xbFFD' || f[1] != u'\o{137775}'				/* { dg-error "delimited escape sequences are only valid in" } */
      || f[2] != f[1])
    __builtin_abort ();
  if (g[0] != L'\u1234' || g[0] != L'\u{1234}'					/* { dg-error "delimited escape sequences are only valid in" } */
      || g[1] != L'\U0000BFFD' || g[1] != L'\u{00000bFfD}'			/* { dg-error "delimited escape sequences are only valid in" } */
      || g[2] != g[0]
      || h[0] != L'\x1234' || h[0] != L'\x{001234}'				/* { dg-error "delimited escape sequences are only valid in" } */
      || h[1] != L'\xBFFD' || h[1] != L'\x{00bFfD}'				/* { dg-error "delimited escape sequences are only valid in" } */
      || h[2] != h[0]
      || i[0] != L'\x29c' || i[0] != L'\o{001234}'				/* { dg-error "delimited escape sequences are only valid in" } */
      || i[1] != L'\xbFFD' || i[1] != L'\o{137775}'				/* { dg-error "delimited escape sequences are only valid in" } */
      || i[2] != i[1])
    __builtin_abort ();
  if (k[0] != '\x034' || k[0] != '\x{0034}'					/* { dg-error "delimited escape sequences are only valid in" } */
      || k[1] != '\x3D' || k[1] != '\x{3d}'					/* { dg-error "delimited escape sequences are only valid in" } */
      || l[0] != '\x1c' || l[0] != '\o{0034}'					/* { dg-error "delimited escape sequences are only valid in" } */
      || l[1] != '\x07e' || l[1] != '\o{176}' || l[1] != '\176')		/* { dg-error "delimited escape sequences are only valid in" } */
    __builtin_abort ();
  return 0;
}
