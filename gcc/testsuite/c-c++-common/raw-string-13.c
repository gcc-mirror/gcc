// PR preprocessor/57620
// { dg-do run }
// { dg-require-effective-target wchar }
// { dg-options "-std=gnu99 -Wno-c++-compat -trigraphs" { target c } }
// { dg-options "-std=c++11" { target c++ } }

#ifndef __cplusplus
#include <wchar.h>

typedef __CHAR16_TYPE__	char16_t;
typedef __CHAR32_TYPE__ char32_t;
#endif

#define R
#define u
#define uR
#define U
#define UR
#define u8
#define u8R
#define L
#define LR

const char s00[] = R"??=??(??<??>??)??'??!??-\
(a)#[{}]^|~";
)??=??";
const char s01[] = R"a(
)\
a"
)a";
const char s02[] = R"a(
)a\
"
)a";
const char s03[] = R"ab(
)a\
b"
)ab";
const char s04[] = R"a??/(x)a??/";
const char s05[] = R"abcdefghijklmn??(abc)abcdefghijklmn??";
const char s06[] = R"abcdefghijklm??/(abc)abcdefghijklm??/";
const char s07[] = R"abc(??)\
abc";)abc";
const char s08[] = R"def(de)\
def";)def";
const char s09[] = R"a(??)\
a"
)a";
const char s10[] = R"a(??)a\
"
)a";
const char s11[] = R"ab(??)a\
b"
)ab";
const char s12[] = R"a#(a#)a??=)a#";
const char s13[] = R"a#(??)a??=??)a#";
const char s14[] = R"??/(x)??/
";)??/";
const char s15[] = R"??/(??)??/
";)??/";
const char s16[] = R"??(??)??";
const char s17[] = R"?(?)??)?";
const char s18[] = R"??(??)??)??)??";

const char u800[] = u8R"??=??(??<??>??)??'??!??-\
(a)#[{}]^|~";
)??=??";
const char u801[] = u8R"a(
)\
a"
)a";
const char u802[] = u8R"a(
)a\
"
)a";
const char u803[] = u8R"ab(
)a\
b"
)ab";
const char u804[] = u8R"a??/(x)a??/";
const char u805[] = u8R"abcdefghijklmn??(abc)abcdefghijklmn??";
const char u806[] = u8R"abcdefghijklm??/(abc)abcdefghijklm??/";
const char u807[] = u8R"abc(??)\
abc";)abc";
const char u808[] = u8R"def(de)\
def";)def";
const char u809[] = u8R"a(??)\
a"
)a";
const char u810[] = u8R"a(??)a\
"
)a";
const char u811[] = u8R"ab(??)a\
b"
)ab";
const char u812[] = u8R"a#(a#)a??=)a#";
const char u813[] = u8R"a#(??)a??=??)a#";
const char u814[] = u8R"??/(x)??/
";)??/";
const char u815[] = u8R"??/(??)??/
";)??/";
const char u816[] = u8R"??(??)??";
const char u817[] = u8R"?(?)??)?";
const char u818[] = u8R"??(??)??)??)??";

const char16_t u00[] = uR"??=??(??<??>??)??'??!??-\
(a)#[{}]^|~";
)??=??";
const char16_t u01[] = uR"a(
)\
a"
)a";
const char16_t u02[] = uR"a(
)a\
"
)a";
const char16_t u03[] = uR"ab(
)a\
b"
)ab";
const char16_t u04[] = uR"a??/(x)a??/";
const char16_t u05[] = uR"abcdefghijklmn??(abc)abcdefghijklmn??";
const char16_t u06[] = uR"abcdefghijklm??/(abc)abcdefghijklm??/";
const char16_t u07[] = uR"abc(??)\
abc";)abc";
const char16_t u08[] = uR"def(de)\
def";)def";
const char16_t u09[] = uR"a(??)\
a"
)a";
const char16_t u10[] = uR"a(??)a\
"
)a";
const char16_t u11[] = uR"ab(??)a\
b"
)ab";
const char16_t u12[] = uR"a#(a#)a??=)a#";
const char16_t u13[] = uR"a#(??)a??=??)a#";
const char16_t u14[] = uR"??/(x)??/
";)??/";
const char16_t u15[] = uR"??/(??)??/
";)??/";
const char16_t u16[] = uR"??(??)??";
const char16_t u17[] = uR"?(?)??)?";
const char16_t u18[] = uR"??(??)??)??)??";

const char32_t U00[] = UR"??=??(??<??>??)??'??!??-\
(a)#[{}]^|~";
)??=??";
const char32_t U01[] = UR"a(
)\
a"
)a";
const char32_t U02[] = UR"a(
)a\
"
)a";
const char32_t U03[] = UR"ab(
)a\
b"
)ab";
const char32_t U04[] = UR"a??/(x)a??/";
const char32_t U05[] = UR"abcdefghijklmn??(abc)abcdefghijklmn??";
const char32_t U06[] = UR"abcdefghijklm??/(abc)abcdefghijklm??/";
const char32_t U07[] = UR"abc(??)\
abc";)abc";
const char32_t U08[] = UR"def(de)\
def";)def";
const char32_t U09[] = UR"a(??)\
a"
)a";
const char32_t U10[] = UR"a(??)a\
"
)a";
const char32_t U11[] = UR"ab(??)a\
b"
)ab";
const char32_t U12[] = UR"a#(a#)a??=)a#";
const char32_t U13[] = UR"a#(??)a??=??)a#";
const char32_t U14[] = UR"??/(x)??/
";)??/";
const char32_t U15[] = UR"??/(??)??/
";)??/";
const char32_t U16[] = UR"??(??)??";
const char32_t U17[] = UR"?(?)??)?";
const char32_t U18[] = UR"??(??)??)??)??";

const wchar_t L00[] = LR"??=??(??<??>??)??'??!??-\
(a)#[{}]^|~";
)??=??";
const wchar_t L01[] = LR"a(
)\
a"
)a";
const wchar_t L02[] = LR"a(
)a\
"
)a";
const wchar_t L03[] = LR"ab(
)a\
b"
)ab";
const wchar_t L04[] = LR"a??/(x)a??/";
const wchar_t L05[] = LR"abcdefghijklmn??(abc)abcdefghijklmn??";
const wchar_t L06[] = LR"abcdefghijklm??/(abc)abcdefghijklm??/";
const wchar_t L07[] = LR"abc(??)\
abc";)abc";
const wchar_t L08[] = LR"def(de)\
def";)def";
const wchar_t L09[] = LR"a(??)\
a"
)a";
const wchar_t L10[] = LR"a(??)a\
"
)a";
const wchar_t L11[] = LR"ab(??)a\
b"
)ab";
const wchar_t L12[] = LR"a#(a#)a??=)a#";
const wchar_t L13[] = LR"a#(??)a??=??)a#";
const wchar_t L14[] = LR"??/(x)??/
";)??/";
const wchar_t L15[] = LR"??/(??)??/
";)??/";
const wchar_t L16[] = LR"??(??)??";
const wchar_t L17[] = LR"?(?)??)?";
const wchar_t L18[] = LR"??(??)??)??)??";

int
main (void)
{
#define TEST(str, val) \
  if (sizeof (str) != sizeof (val) \
      || __builtin_memcmp (str, val, sizeof (str)) != 0) \
    __builtin_abort ()
  TEST (s00, "??""<??"">??"")??""'??""!??""-\\\n(a)#[{}]^|~\";\n");
  TEST (s01, "\n)\\\na\"\n");
  TEST (s02, "\n)a\\\n\"\n");
  TEST (s03, "\n)a\\\nb\"\n");
  TEST (s04, "x");
  TEST (s05, "abc");
  TEST (s06, "abc");
  TEST (s07, "??"")\\\nabc\";");
  TEST (s08, "de)\\\ndef\";");
  TEST (s09, "??"")\\\na\"\n");
  TEST (s10, "??"")a\\\n\"\n");
  TEST (s11, "??"")a\\\nb\"\n");
  TEST (s12, "a#)a??""=");
  TEST (s13, "??"")a??""=??");
  TEST (s14, "x)??""/\n\";");
  TEST (s15, "??"")??""/\n\";");
  TEST (s16, "??");
  TEST (s17, "?)??");
  TEST (s18, "??"")??"")??");
  TEST (u800, u8"??""<??"">??"")??""'??""!??""-\\\n(a)#[{}]^|~\";\n");
  TEST (u801, u8"\n)\\\na\"\n");
  TEST (u802, u8"\n)a\\\n\"\n");
  TEST (u803, u8"\n)a\\\nb\"\n");
  TEST (u804, u8"x");
  TEST (u805, u8"abc");
  TEST (u806, u8"abc");
  TEST (u807, u8"??"")\\\nabc\";");
  TEST (u808, u8"de)\\\ndef\";");
  TEST (u809, u8"??"")\\\na\"\n");
  TEST (u810, u8"??"")a\\\n\"\n");
  TEST (u811, u8"??"")a\\\nb\"\n");
  TEST (u812, u8"a#)a??""=");
  TEST (u813, u8"??"")a??""=??");
  TEST (u814, u8"x)??""/\n\";");
  TEST (u815, u8"??"")??""/\n\";");
  TEST (u816, u8"??");
  TEST (u817, u8"?)??");
  TEST (u818, u8"??"")??"")??");
  TEST (u00, u"??""<??"">??"")??""'??""!??""-\\\n(a)#[{}]^|~\";\n");
  TEST (u01, u"\n)\\\na\"\n");
  TEST (u02, u"\n)a\\\n\"\n");
  TEST (u03, u"\n)a\\\nb\"\n");
  TEST (u04, u"x");
  TEST (u05, u"abc");
  TEST (u06, u"abc");
  TEST (u07, u"??"")\\\nabc\";");
  TEST (u08, u"de)\\\ndef\";");
  TEST (u09, u"??"")\\\na\"\n");
  TEST (u10, u"??"")a\\\n\"\n");
  TEST (u11, u"??"")a\\\nb\"\n");
  TEST (u12, u"a#)a??""=");
  TEST (u13, u"??"")a??""=??");
  TEST (u14, u"x)??""/\n\";");
  TEST (u15, u"??"")??""/\n\";");
  TEST (u16, u"??");
  TEST (u17, u"?)??");
  TEST (u18, u"??"")??"")??");
  TEST (U00, U"??""<??"">??"")??""'??""!??""-\\\n(a)#[{}]^|~\";\n");
  TEST (U01, U"\n)\\\na\"\n");
  TEST (U02, U"\n)a\\\n\"\n");
  TEST (U03, U"\n)a\\\nb\"\n");
  TEST (U04, U"x");
  TEST (U05, U"abc");
  TEST (U06, U"abc");
  TEST (U07, U"??"")\\\nabc\";");
  TEST (U08, U"de)\\\ndef\";");
  TEST (U09, U"??"")\\\na\"\n");
  TEST (U10, U"??"")a\\\n\"\n");
  TEST (U11, U"??"")a\\\nb\"\n");
  TEST (U12, U"a#)a??""=");
  TEST (U13, U"??"")a??""=??");
  TEST (U14, U"x)??""/\n\";");
  TEST (U15, U"??"")??""/\n\";");
  TEST (U16, U"??");
  TEST (U17, U"?)??");
  TEST (U18, U"??"")??"")??");
  TEST (L00, L"??""<??"">??"")??""'??""!??""-\\\n(a)#[{}]^|~\";\n");
  TEST (L01, L"\n)\\\na\"\n");
  TEST (L02, L"\n)a\\\n\"\n");
  TEST (L03, L"\n)a\\\nb\"\n");
  TEST (L04, L"x");
  TEST (L05, L"abc");
  TEST (L06, L"abc");
  TEST (L07, L"??"")\\\nabc\";");
  TEST (L08, L"de)\\\ndef\";");
  TEST (L09, L"??"")\\\na\"\n");
  TEST (L10, L"??"")a\\\n\"\n");
  TEST (L11, L"??"")a\\\nb\"\n");
  TEST (L12, L"a#)a??""=");
  TEST (L13, L"??"")a??""=??");
  TEST (L14, L"x)??""/\n\";");
  TEST (L15, L"??"")??""/\n\";");
  TEST (L16, L"??");
  TEST (L17, L"?)??");
  TEST (L18, L"??"")??"")??");
  return 0;
}
