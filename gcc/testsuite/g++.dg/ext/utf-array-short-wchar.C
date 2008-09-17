/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Expected errors for char16_t/char32_t string literals. */
/* { dg-do compile } */
/* { dg-options "-std=c++0x -fshort-wchar" } */

const char	s_0[]	= "ab";
const char	s_1[]	= u"ab";	/* { dg-error "from wide string" } */
const char	s_2[]	= U"ab";	/* { dg-error "from wide string" } */
const char	s_3[]	= L"ab";	/* { dg-error "from wide string" } */

const char16_t	s16_0[]	= "ab";		/* { dg-error "from non-wide" } */
const char16_t	s16_1[]	= u"ab";
const char16_t	s16_2[]	= U"ab";	/* { dg-error "from incompatible" } */
const char16_t	s16_3[]	= L"ab";	/* { dg-error "from incompatible" } */

const char16_t	s16_4[0] = u"ab";	/* { dg-error "chars is too long" } */
const char16_t	s16_5[1] = u"ab";	/* { dg-error "chars is too long" } */
const char16_t	s16_6[2] = u"ab";	/* { dg-error "chars is too long" } */
const char16_t	s16_7[3] = u"ab";
const char16_t	s16_8[4] = u"ab";

const char32_t	s32_0[]	= "ab";		/* { dg-error "from non-wide" } */
const char32_t	s32_1[]	= u"ab";	/* { dg-error "from incompatible" } */
const char32_t	s32_2[]	= U"ab";
const char32_t	s32_3[]	= L"ab";	/* { dg-error "from incompatible" } */

const char32_t	s32_4[0] = U"ab";	/* { dg-error "chars is too long" } */
const char32_t	s32_5[1] = U"ab";	/* { dg-error "chars is too long" } */
const char32_t	s32_6[2] = U"ab";	/* { dg-error "chars is too long" } */
const char32_t	s32_7[3] = U"ab";
const char32_t	s32_8[4] = U"ab";

const wchar_t	sw_0[]	= "ab";		/* { dg-error "from non-wide" } */
const wchar_t	sw_1[]	= u"ab";	/* { dg-error "from incompatible" } */
const wchar_t	sw_2[]	= U"ab";	/* { dg-error "from incompatible" } */
const wchar_t	sw_3[]	= L"ab";
