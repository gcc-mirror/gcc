// PR c++/48570
// { dg-do compile { target c++11 } }

constexpr wchar_t c1 = L"hi"[3];	// { dg-error "array subscript" }
constexpr char16_t c2 = u"hi"[3];	// { dg-error "array subscript" }
constexpr char32_t c3 = U"hi"[3];	// { dg-error "array subscript" }
