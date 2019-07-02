// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

template <typename T> class TPL
{
  template <typename U> friend void foo (U);
};

template class TPL<char>;

template <typename V> void foo (V x)
{
}
