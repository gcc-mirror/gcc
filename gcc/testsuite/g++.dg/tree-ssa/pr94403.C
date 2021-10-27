// PR tree-optimization/94403
// Only test on some 64-bit targets which do have bswap{si,di}2 patterns and
// are either big or little endian (not pdp endian).
// { dg-do compile { target { lp64 && { i?86-*-* x86_64-*-* powerpc*-*-* aarch64*-*-* } } } }
// { dg-require-effective-target store_merge }
// { dg-options "-O2 -fno-tree-vectorize -fdump-tree-store-merging -std=c++17" }

namespace std {
  template <typename T>
  void swap (T& t1, T& t2) { T tmp (t1); t1 = t2; t2 = tmp; }
  enum class byte : unsigned char {};
}

unsigned
bswap32 (unsigned int x)
{
  unsigned int y = x;
  std::byte *bytes = reinterpret_cast<std::byte*> (&y);
  std::swap (bytes[0], bytes[3]);
  std::swap (bytes[1], bytes[2]);
  return y;
}

unsigned long long
bswap64 (unsigned long long x)
{
  unsigned long long y = x;
  std::byte *bytes = reinterpret_cast<std::byte*> (&y);
  std::swap (bytes[0], bytes[7]);
  std::swap (bytes[1], bytes[6]);
  std::swap (bytes[2], bytes[5]);
  std::swap (bytes[3], bytes[4]);
  return y;
}

/* { dg-final { scan-tree-dump-times "__builtin_bswap64" 1 "store-merging" } } */
/* { dg-final { scan-tree-dump-times "__builtin_bswap32" 1 "store-merging" } } */
