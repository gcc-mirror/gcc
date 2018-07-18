// PR sanitizer/81111
// { dg-do compile }
// { dg-options "-fsanitize=shift" }

template <typename V>
struct N
{
  static const V m = (((V)(-1) < 0)
		      ? (V)1 << (sizeof(V) * __CHAR_BIT__ - ((V)(-1) < 0))
		      : (V) 0);
};

template<typename V>
const V N<V>::m;

template <typename V>
struct O
{
  static const V m = (V)1 << sizeof(V) * __CHAR_BIT__;
};

template<typename V>
const V O<V>::m;

void
foo ()
{
  N<long long>::m;
  N<unsigned long long>::m;
#ifdef __SIZEOF_INT128__
  N<__int128>::m;
  N<unsigned __int128>::m;
#endif
}

void
bar ()
{
  O<long long>::m;
  O<unsigned long long>::m;
#ifdef __SIZEOF_INT128__
  O<__int128>::m;
  O<unsigned __int128>::m;
#endif
}
