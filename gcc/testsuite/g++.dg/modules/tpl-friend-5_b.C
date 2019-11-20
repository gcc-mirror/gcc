// { dg-additional-options "-fmodules-ts -fdump-lang-module" }

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
// { dg-final { scan-lang-dump {Loading entity foo\[0\] section:1} module } }
// { dg-final { scan-lang-dump {Loading entity foo\[1\] section:2} module } }
