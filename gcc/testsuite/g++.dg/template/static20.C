// PR c++/24277

template< int Bits > struct uint_t {
  typedef unsigned short fast;
};
template < int Bits > struct mask_uint_t {
  typedef typename uint_t< Bits >::fast fast;
  static const fast sig_bits = 1;
  static const fast sig_bits_fast = fast(sig_bits);
};
template < int Bits> int checksum ( ) {
  return 1 & mask_uint_t<Bits>::sig_bits_fast;
}
int i = checksum<1>();
