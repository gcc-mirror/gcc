// PR c++/93549
// { dg-do compile { target c++17 } }
// { dg-options "-O2 -Wno-psabi -w" }

struct simd {
  using shortx8 [[gnu::vector_size(16)]] = short;
  shortx8 data;
  constexpr simd (short x) : data{x, x, x, x, x, x, x, x} {}
  constexpr friend unsigned operator== (simd lhs, simd rhs)
  {
    shortx8 tmp = lhs.data == rhs.data;
    using ushort = unsigned short;
    auto bools = tmp ? ushort(1) : ushort(0);
    unsigned bits = 0;
    for (int i = 0; i < 8; ++i)
      bits |= bools[i] << i;
    return bits;
  }
};

auto
foo ()
{
  constexpr auto tmp = simd(1) == simd(2);
  return tmp;
}
