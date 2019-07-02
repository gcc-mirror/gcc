// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

template<typename _CharT> class basic_ios;

template<typename _CharT>
class basic_streambuf
{
  friend class basic_ios<_CharT>;

  _CharT member;
};
