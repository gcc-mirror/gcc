/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Expected errors for char16_t/char32_t string literals. */
/* { dg-do compile { target c++11 } } */
// { dg-options "" }

#if __cpp_char8_t
typedef char8_t u8_char_t;
#else
typedef char u8_char_t;
#endif

const char	s_0[]	= "ab";
const char	s_1[]	= u"ab";	/* { dg-error "from a string literal with type array of .char16_t." } */
const char	s_2[]	= U"ab";	/* { dg-error "from a string literal with type array of .char32_t." } */
const char	s_3[]	= L"ab";	/* { dg-error "from a string literal with type array of .wchar_t." } */
const u8_char_t	s_4[]	= u8"ab";

const char16_t	s16_0[]	= "ab";		/* { dg-error "from a string literal with type array of .char." } */
const char16_t	s16_1[]	= u"ab";
const char16_t	s16_2[]	= U"ab";	/* { dg-error "from a string literal with type array of .char32_t." } */
const char16_t	s16_3[]	= L"ab";	/* { dg-error "from a string literal with type array of .wchar_t." } */
const char16_t	s16_4[]	= u8"ab";	/* { dg-error "from a string literal with type array of .char." } */

const char16_t	s16_5[0] = u"ab";	/* { dg-error "initializer-string for 'const char16_t \\\[0]' is too long" } */
const char16_t	s16_6[1] = u"ab";	/* { dg-error "initializer-string for 'const char16_t \\\[1]' is too long" } */
const char16_t	s16_7[2] = u"ab";	/* { dg-error "initializer-string for 'const char16_t \\\[2]' is too long" } */
const char16_t	s16_8[3] = u"ab";
const char16_t	s16_9[4] = u"ab";

const char32_t	s32_0[]	= "ab";		/* { dg-error "from a string literal with type array of .char." } */
const char32_t	s32_1[]	= u"ab";	/* { dg-error "from a string literal with type array of .char16_t." } */
const char32_t	s32_2[]	= U"ab";
const char32_t	s32_3[]	= L"ab";	/* { dg-error "from a string literal with type array of .wchar_t." } */
const char32_t	s32_4[]	= u8"ab";	/* { dg-error "from a string literal with type array of .char." } */

const char32_t	s32_5[0] = U"ab";	/* { dg-error "initializer-string for 'const char32_t \\\[0]' is too long" } */
const char32_t	s32_6[1] = U"ab";	/* { dg-error "initializer-string for 'const char32_t \\\[1]' is too long" } */
const char32_t	s32_7[2] = U"ab";	/* { dg-error "initializer-string for 'const char32_t \\\[2]' is too long" } */
const char32_t	s32_8[3] = U"ab";
const char32_t	s32_9[4] = U"ab";

const wchar_t	sw_0[]	= "ab";		/* { dg-error "from a string literal with type array of .char." } */
const wchar_t	sw_1[]	= u"ab";	/* { dg-error "from a string literal with type array of .char16_t." } */
const wchar_t	sw_2[]	= U"ab";	/* { dg-error "from a string literal with type array of .char32_t." } */
const wchar_t	sw_3[]	= L"ab";
const wchar_t	sw_4[]	= u8"ab";	/* { dg-error "from a string literal with type array of .char." } */
