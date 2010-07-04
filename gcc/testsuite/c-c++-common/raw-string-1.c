// { dg-do run }
// { dg-require-effective-target wchar }
// { dg-options "-std=gnu99 -Wno-c++-compat -trigraphs" { target c } }
// { dg-options "-std=c++0x" { target c++ } }

#ifndef __cplusplus
#include <wchar.h>

typedef __CHAR16_TYPE__	char16_t;
typedef __CHAR32_TYPE__ char32_t;
#endif

const char s0[] = R"(a\
\u010d\U0000010D\\\'\"\?\a\b\f\n\r\t\v\0\00\000\xa\xabb
c)";
const char s1[] = "a\\\n\\u010d\\U0000010D\\\\\\'\\\"\\?\\a\\b\\f\\n\\r\\t\\v\\0\\00\\000\\xa\\xabb\nc";
const char s2[] = R"*|*(a\
b
c)"
c)*|"
c)*|*";
const char s3[] = "a\\\nb\nc)\"\nc)*|\"\nc";
// The ) in ??) below is part of the raw string suffix )".
const char s4[] = R"(??/
??/
??(??<??=??'??!??-??>??)";
const char s5[] = "?\?/\n?\?/\n?\?(?\?<?\?=?\?'?\?!?\?-?\?>?\?";

const char t0[] = u8R"(a\
\u010d\U0000010D\\\'\"\?\a\b\f\n\r\t\v\0\00\000\xa\xabb
c)";
const char t1[] = u8"a\\\n\\u010d\\U0000010D\\\\\\'\\\"\\?\\a\\b\\f\\n\\r\\t\\v\\0\\00\\000\\xa\\xabb\nc";
const char t2[] = u8R"*|*(a\
b
c)"
c)*|"
c)*|*";
const char t3[] = u8"a\\\nb\nc)\"\nc)*|\"\nc";
const char t4[] = u8R"(??/
??/
??(??<??=??'??!??-??>??)";
const char t5[] = u8"?\?/\n?\?/\n?\?(?\?<?\?=?\?'?\?!?\?-?\?>?\?";

const char16_t u0[] = uR"(a\
\u010d\U0000010D\\\'\"\?\a\b\f\n\r\t\v\0\00\000\xa\xabb
c)";
const char16_t u1[] = u"a\\\n\\u010d\\U0000010D\\\\\\'\\\"\\?\\a\\b\\f\\n\\r\\t\\v\\0\\00\\000\\xa\\xabb\nc";
const char16_t u2[] = uR"*|*(a\
b
c)"
c)*|"
c)*|*";
const char16_t u3[] = u"a\\\nb\nc)\"\nc)*|\"\nc";
const char16_t u4[] = uR"(??/
??/
??(??<??=??'??!??-??>??)";
const char16_t u5[] = u"?\?/\n?\?/\n?\?(?\?<?\?=?\?'?\?!?\?-?\?>?\?";

const char32_t U0[] = UR"(a\
\u010d\U0000010D\\\'\"\?\a\b\f\n\r\t\v\0\00\000\xa\xabb
c)";
const char32_t U1[] = U"a\\\n\\u010d\\U0000010D\\\\\\'\\\"\\?\\a\\b\\f\\n\\r\\t\\v\\0\\00\\000\\xa\\xabb\nc";
const char32_t U2[] = UR"*|*(a\
b
c)"
c)*|"
c)*|*";
const char32_t U3[] = U"a\\\nb\nc)\"\nc)*|\"\nc";
const char32_t U4[] = UR"(??/
??/
??(??<??=??'??!??-??>??)";
const char32_t U5[] = U"?\?/\n?\?/\n?\?(?\?<?\?=?\?'?\?!?\?-?\?>?\?";

const wchar_t L0[] = LR"(a\
\u010d\U0000010D\\\'\"\?\a\b\f\n\r\t\v\0\00\000\xa\xabb
c)";
const wchar_t L1[] = L"a\\\n\\u010d\\U0000010D\\\\\\'\\\"\\?\\a\\b\\f\\n\\r\\t\\v\\0\\00\\000\\xa\\xabb\nc";
const wchar_t L2[] = LR"*|*(a\
b
c)"
c)*|"
c)*|*";
const wchar_t L3[] = L"a\\\nb\nc)\"\nc)*|\"\nc";
const wchar_t L4[] = LR"(??/
??/
??(??<??=??'??!??-??>??)";
const wchar_t L5[] = L"?\?/\n?\?/\n?\?(?\?<?\?=?\?'?\?!?\?-?\?>?\?";

int
main (void)
{
  if (sizeof (s0) != sizeof (s1)
      || __builtin_memcmp (s0, s1, sizeof (s0)) != 0)
    __builtin_abort ();
  if (sizeof (s2) != sizeof (s3)
      || __builtin_memcmp (s2, s3, sizeof (s2)) != 0)
    __builtin_abort ();
  if (sizeof (s4) != sizeof (s5)
      || __builtin_memcmp (s4, s5, sizeof (s4)) != 0)
    __builtin_abort ();
  if (sizeof (t0) != sizeof (t1)
      || __builtin_memcmp (t0, t1, sizeof (t0)) != 0)
    __builtin_abort ();
  if (sizeof (t2) != sizeof (t3)
      || __builtin_memcmp (t2, t3, sizeof (t2)) != 0)
    __builtin_abort ();
  if (sizeof (t4) != sizeof (t5)
      || __builtin_memcmp (t4, t5, sizeof (t4)) != 0)
    __builtin_abort ();
  if (sizeof (u0) != sizeof (u1)
      || __builtin_memcmp (u0, u1, sizeof (u0)) != 0)
    __builtin_abort ();
  if (sizeof (u2) != sizeof (u3)
      || __builtin_memcmp (u2, u3, sizeof (u2)) != 0)
    __builtin_abort ();
  if (sizeof (u4) != sizeof (u5)
      || __builtin_memcmp (u4, u5, sizeof (u4)) != 0)
    __builtin_abort ();
  if (sizeof (U0) != sizeof (U1)
      || __builtin_memcmp (U0, U1, sizeof (U0)) != 0)
    __builtin_abort ();
  if (sizeof (U2) != sizeof (U3)
      || __builtin_memcmp (U2, U3, sizeof (U2)) != 0)
    __builtin_abort ();
  if (sizeof (U4) != sizeof (U5)
      || __builtin_memcmp (U4, U5, sizeof (U4)) != 0)
    __builtin_abort ();
  if (sizeof (L0) != sizeof (L1)
      || __builtin_memcmp (L0, L1, sizeof (L0)) != 0)
    __builtin_abort ();
  if (sizeof (L2) != sizeof (L3)
      || __builtin_memcmp (L2, L3, sizeof (L2)) != 0)
    __builtin_abort ();
  if (sizeof (L4) != sizeof (L5)
      || __builtin_memcmp (L4, L5, sizeof (L4)) != 0)
    __builtin_abort ();
  if (sizeof (R"*()*") != 1
      || __builtin_memcmp (R"*()*", "", 1) != 0)
    __builtin_abort ();
  return 0;
}
