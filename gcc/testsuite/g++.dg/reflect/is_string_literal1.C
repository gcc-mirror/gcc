// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::is_string_literal.

#include <meta>

constexpr char a = '\0';
constexpr wchar_t b = L'\0';
constexpr char16_t c = u'\0';
constexpr char32_t d = U'\0';
constexpr char8_t e = u8'\0';
constexpr char f[] = "foobar";
static_assert (!std::is_string_literal (&a));
static_assert (!std::is_string_literal (&b));
static_assert (!std::is_string_literal (&c));
static_assert (!std::is_string_literal (&d));
static_assert (!std::is_string_literal (&e));
static_assert (!std::is_string_literal (&f[0]));
static_assert (!std::is_string_literal (f + 2));
static_assert (!std::is_string_literal (&f[2] + 3));
static_assert (std::is_string_literal ("abcd"));
static_assert (std::is_string_literal (&"abcd"[4]));
static_assert (std::is_string_literal ("abcd" + 2));
static_assert (std::is_string_literal (&"abcd"[1] + 1));
static_assert (std::is_string_literal (L"foobar"));
static_assert (std::is_string_literal (&L"foobar"[4]));
static_assert (std::is_string_literal (L"foobar" + 2));
static_assert (std::is_string_literal (&L"foobar"[1] + 1));
static_assert (std::is_string_literal (u"corge"));
static_assert (std::is_string_literal (&u"corge"[4]));
static_assert (std::is_string_literal (u"corge" + 2));
static_assert (std::is_string_literal (&u"corge"[1] + 1));
static_assert (std::is_string_literal (U"garply"));
static_assert (std::is_string_literal (&U"garply"[4]));
static_assert (std::is_string_literal (U"garply" + 2));
static_assert (std::is_string_literal (&U"garply"[1] + 1));
static_assert (std::is_string_literal (u8"Certe, inquam, pértinax non ero tibíque, si mihi probábis ea, quæ dices, libénter asséntiar."));
static_assert (std::is_string_literal (&u8"Certe, inquam, pértinax non ero tibíque, si mihi probábis ea, quæ dices, libénter asséntiar."[4]));
static_assert (std::is_string_literal (u8"Certe, inquam, pértinax non ero tibíque, si mihi probábis ea, quæ dices, libénter asséntiar." + 2));
static_assert (std::is_string_literal (&u8"Certe, inquam, pértinax non ero tibíque, si mihi probábis ea, quæ dices, libénter asséntiar."[1] + 1));
