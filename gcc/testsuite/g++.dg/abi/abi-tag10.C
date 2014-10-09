#ifndef ABI_TAG
#define ABI_TAG __attribute__((__abi_tag__("cxx11")))
#endif

typedef unsigned long size_t;

template<typename C>
struct char_traits { };
template<typename C>
struct allocator { };

template<typename C, typename T = char_traits<C>, typename A = allocator<C> >
struct ABI_TAG basic_string { };

typedef basic_string<char> string;

template<typename T>
struct hash
{
  size_t
  operator()(T val) const;
};

template<>
size_t
hash<string>::operator()(string) const { return 0; }

// { dg-final { scan-assembler "_ZNK4hashI12basic_stringB5cxx11Ic11char_traitsIcE9allocatorIcEEEclES5_" } }
