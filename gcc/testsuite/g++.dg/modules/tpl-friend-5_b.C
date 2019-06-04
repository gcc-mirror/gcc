// { dg-additional-options -fmodules-ts }

module foo;

template class basic_streambuf<char>;

template<typename _CharT> class basic_ios
{
public:
  static void frob (basic_streambuf<_CharT> *p, _CharT val)
  {
    p->member = val;
  }
};

void bill (basic_streambuf<char> *p)
{
  basic_ios<char>::frob (p, 5);
}

// { dg-final { scan-lang-dump {Lazily binding '::basic_streambuf'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Lazily binding '::basic_ios'@'foo' section:} module } }
// { dg-final { scan-lang-dump {Lazily binding '#null##null#'@'foo' section:} module } }
