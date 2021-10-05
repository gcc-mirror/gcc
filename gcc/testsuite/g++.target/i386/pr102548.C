// PR c++/102548
// { dg-do compile { target { c++14 && ia32 } } }

typedef decltype(sizeof(0)) size_t;
struct tm;
extern "C" size_t __attribute__((__cdecl__)) strftime (char *, size_t, const char *, const struct tm *);

auto
foo (void)
{
  return strftime;
}
