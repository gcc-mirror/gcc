// PR c++/48570
// { dg-do compile }
// { dg-options -std=c++11 }

constexpr wchar_t c1 = L"hi"[3];	// { dg-error "out of bound" }
constexpr char16_t c2 = u"hi"[3];	// { dg-error "out of bound" }
constexpr char32_t c3 = U"hi"[3];	// { dg-error "out of bound" }
