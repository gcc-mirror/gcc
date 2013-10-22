// { dg-options "-std=c++11" }
// PR c++/50941

typedef decltype(sizeof(0)) size_type;

constexpr size_type
operator"" _len(const char*, size_type len)
{
  return len;
}

constexpr size_type
operator"" _len(const wchar_t*, size_type len)
{
  return len;
}

constexpr size_type
operator"" _len(const char16_t*, size_type len)
{
  return len;
}

constexpr size_type
operator"" _len(const char32_t*, size_type len)
{
  return len;
}

static_assert(  ""_len == 0, "Ouch");
static_assert(u8""_len == 0, "Ouch");
static_assert( L""_len == 0, "Ouch");
static_assert( u""_len == 0, "Ouch");
static_assert( U""_len == 0, "Ouch");

static_assert(  "1"_len == 1, "Ouch");
static_assert(u8"1"_len == 1, "Ouch");
static_assert( L"1"_len == 1, "Ouch");
static_assert( u"1"_len == 1, "Ouch");
static_assert( U"1"_len == 1, "Ouch");

static_assert(  "123"_len == 3, "Ouch");
static_assert(u8"123"_len == 3, "Ouch");
static_assert( L"123"_len == 3, "Ouch");
static_assert( u"123"_len == 3, "Ouch");
static_assert( U"123"_len == 3, "Ouch");
