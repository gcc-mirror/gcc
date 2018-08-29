// PR c++/67625
// { dg-do compile { target c++11 } }

constexpr unsigned short
bswap16 (unsigned short x)
{
  return __builtin_bswap16 (x);
}
constexpr int a = bswap16 (1);
enum { b = a };
enum { c = __builtin_bswap16 (1) };
enum { d = bswap16 (1) };
