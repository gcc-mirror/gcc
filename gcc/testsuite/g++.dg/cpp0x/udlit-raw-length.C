// { dg-options "-std=c++11" }
// PR c++/50958

typedef decltype(sizeof(0)) size_type;

constexpr size_type
cstrlen_impl(const char* s, size_type i)
{
  return s[i] ? cstrlen_impl(s, i + 1) : i;
}

constexpr size_type
cstrlen(const char* s)
{
  return s ? cstrlen_impl(s, 0) : throw 0;
}

constexpr size_type
operator "" _lenraw(const char* digits)
{
  return cstrlen(digits);
}

static_assert(123_lenraw == 3, "Ouch");
static_assert(1_lenraw == 1, "Ouch");
static_assert(012_lenraw == 3, "Ouch");
static_assert(0_lenraw == 1, "Ouch");
