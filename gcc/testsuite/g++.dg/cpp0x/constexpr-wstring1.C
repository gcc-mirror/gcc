// PR c++/48570
// { dg-do run }
// { dg-options "-std=c++0x" }

extern "C" void abort ();
constexpr wchar_t foo (int i) { return L"0123"[i]; }
constexpr char16_t bar (int i) { return u"0123"[i]; }
constexpr char32_t baz (int i) { return U"0123"[i]; }
const wchar_t foo0 = foo (0);
const wchar_t foo1 = foo (1);
const wchar_t foo2 = foo (2);
const wchar_t foo3 = foo (3);
const wchar_t foo4 = foo (4);
const char16_t bar0 = bar (0);
const char16_t bar1 = bar (1);
const char16_t bar2 = bar (2);
const char16_t bar3 = bar (3);
const char16_t bar4 = bar (4);
const char32_t baz0 = baz (0);
const char32_t baz1 = baz (1);
const char32_t baz2 = baz (2);
const char32_t baz3 = baz (3);
const char32_t baz4 = baz (4);

int
main ()
{
  if (foo0 != L'0' || foo1 != L'1' || foo2 != L'2' || foo3 != L'3' || foo4 != L'\0')
    abort ();
  if (bar0 != u'0' || bar1 != u'1' || bar2 != u'2' || bar3 != u'3' || bar4 != u'\0')
    abort ();
  if (baz0 != U'0' || baz1 != U'1' || baz2 != U'2' || baz3 != U'3' || baz4 != U'\0')
    abort ();
}
