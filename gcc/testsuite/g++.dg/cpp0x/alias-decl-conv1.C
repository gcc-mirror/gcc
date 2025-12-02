// PR c++/122171
// { dg-do compile { target c++11 } }

constexpr unsigned int poly_size(unsigned int bits) {
  return 1;
}

template <unsigned int Deg>
using poly_table = char[poly_size(Deg)];

template <int BITS>
struct FingerprintTable {
  static const poly_table<BITS> table;
};

template <int BITS>
const poly_table<BITS> FingerprintTable<BITS>::table = {};
