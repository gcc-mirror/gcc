// { dg-do run }
// { dg-options "-std=c++0x" }

#define R
#define u
#define uR
#define U
#define UR
#define u8
#define u8R
#define L
#define LR

const char s00[] = R"[a]" "[b]";
const char s01[] = "[a]" R"*[b]*";
const char s02[] = R"[a]" R"[b]";
const char s03[] = R"-[a]-" u8"[b]";
const char s04[] = "[a]" u8R"MNOPQRSTUVWXYZ[b]MNOPQRSTUVWXYZ";
const char s05[] = R"[a]" u8R"wxyzABCDEFGHIJKL[b]wxyzABCDEFGHIJKL";
const char s06[] = u8R";([a];(" "[b]";
const char s07[] = u8"[a]" R"[b]";
const char s08[] = u8R"[a]" R"_{}#()<>%:;.?*+-[b]_{}#()<>%:;.?*+-";
const char s09[] = u8R"/^&|~!=,"'\[a]/^&|~!=,"'\" u8"[b]";
const char s10[] = u8"[a]" u8R"0123456789abcdef[b]0123456789abcdef";
const char s11[] = u8R"ghijklmnopqrstuv[a]ghijklmnopqrstuv" u8R"w[b]w";

const char16_t u03[] = R"-[a]-" u"[b]";
const char16_t u04[] = "[a]" uR"MNOPQRSTUVWXYZ[b]MNOPQRSTUVWXYZ";
const char16_t u05[] = R"[a]" uR"wxyzABCDEFGHIJKL[b]wxyzABCDEFGHIJKL";
const char16_t u06[] = uR";([a];(" "[b]";
const char16_t u07[] = u"[a]" R"[b]";
const char16_t u08[] = uR"[a]" R"_{}#()<>%:;.?*+-[b]_{}#()<>%:;.?*+-";
const char16_t u09[] = uR"/^&|~!=,"'\[a]/^&|~!=,"'\" u"[b]";
const char16_t u10[] = u"[a]" uR"0123456789abcdef[b]0123456789abcdef";
const char16_t u11[] = uR"ghijklmnopqrstuv[a]ghijklmnopqrstuv" uR"w[b]w";

const char32_t U03[] = R"-[a]-" U"[b]";
const char32_t U04[] = "[a]" UR"MNOPQRSTUVWXYZ[b]MNOPQRSTUVWXYZ";
const char32_t U05[] = R"[a]" UR"wxyzABCDEFGHIJKL[b]wxyzABCDEFGHIJKL";
const char32_t U06[] = UR";([a];(" "[b]";
const char32_t U07[] = U"[a]" R"[b]";
const char32_t U08[] = UR"[a]" R"_{}#()<>%:;.?*+-[b]_{}#()<>%:;.?*+-";
const char32_t U09[] = UR"/^&|~!=,"'\[a]/^&|~!=,"'\" U"[b]";
const char32_t U10[] = U"[a]" UR"0123456789abcdef[b]0123456789abcdef";
const char32_t U11[] = UR"ghijklmnopqrstuv[a]ghijklmnopqrstuv" UR"w[b]w";

const wchar_t L03[] = R"-[a]-" L"[b]";
const wchar_t L04[] = "[a]" LR"MNOPQRSTUVWXYZ[b]MNOPQRSTUVWXYZ";
const wchar_t L05[] = R"[a]" LR"wxyzABCDEFGHIJKL[b]wxyzABCDEFGHIJKL";
const wchar_t L06[] = LR";([a];(" "[b]";
const wchar_t L07[] = L"[a]" R"[b]";
const wchar_t L08[] = LR"[a]" R"_{}#()<>%:;.?*+-[b]_{}#()<>%:;.?*+-";
const wchar_t L09[] = LR"/^&|~!=,"'\[a]/^&|~!=,"'\" L"[b]";
const wchar_t L10[] = L"[a]" LR"0123456789abcdef[b]0123456789abcdef";
const wchar_t L11[] = LR"ghijklmnopqrstuv[a]ghijklmnopqrstuv" LR"w[b]w";

int
main (void)
{
#define TEST(str, val) \
  if (sizeof (str) != sizeof (val) \
      || __builtin_memcmp (str, val, sizeof (str)) != 0) \
    __builtin_abort ()
  TEST (s00, "a[b]");
  TEST (s01, "[a]b");
  TEST (s02, "ab");
  TEST (s03, "a[b]");
  TEST (s04, "[a]b");
  TEST (s05, "ab");
  TEST (s06, "a[b]");
  TEST (s07, "[a]b");
  TEST (s08, "ab");
  TEST (s09, "a[b]");
  TEST (s10, "[a]b");
  TEST (s11, "ab");
  TEST (u03, u"a[b]");
  TEST (u04, u"[a]b");
  TEST (u05, u"ab");
  TEST (u06, u"a[b]");
  TEST (u07, u"[a]b");
  TEST (u08, u"ab");
  TEST (u09, u"a[b]");
  TEST (u10, u"[a]b");
  TEST (u11, u"ab");
  TEST (U03, U"a[b]");
  TEST (U04, U"[a]b");
  TEST (U05, U"ab");
  TEST (U06, U"a[b]");
  TEST (U07, U"[a]b");
  TEST (U08, U"ab");
  TEST (U09, U"a[b]");
  TEST (U10, U"[a]b");
  TEST (U11, U"ab");
  TEST (L03, L"a[b]");
  TEST (L04, L"[a]b");
  TEST (L05, L"ab");
  TEST (L06, L"a[b]");
  TEST (L07, L"[a]b");
  TEST (L08, L"ab");
  TEST (L09, L"a[b]");
  TEST (L10, L"[a]b");
  TEST (L11, L"ab");
  return 0;
}
