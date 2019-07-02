// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename _CharT>
static inline __attribute__((__always_inline__)) constexpr void
__constant_char_array_p ()
{
}

inline void foo ()
{
  __constant_char_array_p<char> ();
}
