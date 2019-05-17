/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Expected errors for char16_t/char32_t string literals. */
/* { dg-do compile } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=gnu99" } */

#include <wchar.h>

typedef __CHAR16_TYPE__	char16_t;
typedef __CHAR32_TYPE__	char32_t;

const char	s_0[]	= "ab";
const char	s_1[]	= u"ab";	/* { dg-error "from a string literal with type array of" } */
const char	s_2[]	= U"ab";	/* { dg-error "from a string literal with type array of" } */
const char	s_3[]	= L"ab";	/* { dg-error "from a string literal with type array of .(long |short )?(unsigned )?int." } */
const char	s_4[]	= u8"ab";

const char16_t	s16_0[]	= "ab";		/* { dg-error "from a string literal with type array of .char." } */
const char16_t	s16_1[]	= u"ab";
const char16_t	s16_2[]	= U"ab";	/* { dg-error "from a string literal with type array of" } */
const char16_t	s16_3[]	= L"ab";	/* { dg-error "from a string literal with type array of .(long |short )?(unsigned )?int." "" { target { ! wchar_t_char16_t_compatible } } } */
const char16_t	s16_4[]	= u8"ab";	/* { dg-error "from a string literal with type array of .char." } */

const char16_t	s16_5[0] = u"ab";	/* { dg-warning " is too long" } */
const char16_t	s16_6[1] = u"ab";	/* { dg-warning " is too long" } */
const char16_t	s16_7[2] = u"ab";
const char16_t	s16_8[3] = u"ab";
const char16_t	s16_9[4] = u"ab";

const char32_t	s32_0[]	= "ab";		/* { dg-error "from a string literal with type array of .char." } */
const char32_t	s32_1[]	= u"ab";	/* { dg-error "from a string literal with type array of" } */
const char32_t	s32_2[]	= U"ab";
const char32_t	s32_3[]	= L"ab";	/* { dg-error "from a string literal with type array of .(long |short )?(unsigned )?int." "" { target { ! wchar_t_char32_t_compatible } } } */
const char32_t	s32_4[]	= u8"ab";	/* { dg-error "from a string literal with type array of .char." } */

const char32_t	s32_5[0] = U"ab";	/* { dg-warning " is too long" } */
const char32_t	s32_6[1] = U"ab";	/* { dg-warning " is too long" } */
const char32_t	s32_7[2] = U"ab";	/* { dg-warning " is too long" "" { target "m32c-*-*" } } */
const char32_t	s32_8[3] = U"ab";	/* { dg-warning " is too long" "" { target "m32c-*-*" } } */
const char32_t	s32_9[4] = U"ab";	/* { dg-warning " is too long" "" { target "m32c-*-*" } } */

const wchar_t	sw_0[]	= "ab";		/* { dg-error "from a string literal with type array of .char." } */
const wchar_t	sw_1[]	= u"ab";	/* { dg-error "from a string literal with type array of" "" { target { ! wchar_t_char16_t_compatible } } } */
const wchar_t	sw_2[]	= U"ab";	/* { dg-error "from a string literal with type array of" "" { target { ! wchar_t_char32_t_compatible } } } */
const wchar_t	sw_3[]	= L"ab";
const wchar_t	sw_4[]	= u8"ab";	/* { dg-error "from a string literal with type array of .char." } */
